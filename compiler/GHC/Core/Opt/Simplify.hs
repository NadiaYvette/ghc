{-# LANGUAGE CPP #-}

module GHC.Core.Opt.Simplify
  ( SimplifyExprOpts(..), SimplifyOpts(..)
  , simplifyExpr, simplifyPgm
  ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.Rules
import GHC.Core.Ppr     ( pprCoreBindings, pprCoreExpr )
import GHC.Core.Opt.OccurAnal ( occurAnalysePgm, occurAnalyseExpr )
import GHC.Core.Stats   ( coreBindsSize, coreBindsStats, exprSize )
import GHC.Core.Utils   ( mkTicks, stripTicksTop )
import GHC.Core.Lint    ( LintPassResultConfig, dumpPassResult, lintPassResult )
import GHC.Core.Opt.Simplify.Iteration ( simplTopBinds, simplTopBindsIncr, simplExpr, simplImpRules )
import GHC.Core.Opt.Simplify.Utils  ( activeRule )
import GHC.Core.Opt.Simplify.Inline ( activeUnfolding )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats ( simplCountN )
import GHC.Core.FamInstEnv
import GHC.Core.FVs       ( exprFreeIds )

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)

import GHC.Unit.Env ( UnitEnv, ueEPS )
import GHC.Unit.External
import GHC.Unit.Module.ModGuts

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.InlinePragma
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Tickish
import GHC.Types.Unique.FM

import Control.Monad
import Data.Foldable ( for_ )
import Data.IORef

{-
************************************************************************
*                                                                      *
        Gentle simplification
*                                                                      *
************************************************************************
-}

-- | Configuration record for `simplifyExpr`.
-- The values of this datatype are /only/ driven by the demands of that function.
data SimplifyExprOpts = SimplifyExprOpts
  { se_fam_inst :: ![FamInst]
  , se_mode :: !SimplMode
  , se_top_env_cfg :: !TopEnvConfig
  }

simplifyExpr :: Logger
             -> ExternalUnitCache
             -> SimplifyExprOpts
             -> CoreExpr
             -> IO CoreExpr
-- ^ Simplify an expression using 'simplExprGently'.
--
-- See 'simplExprGently' for details.
simplifyExpr logger euc opts expr
  = withTiming logger (text "Simplify [expr]") (const ()) $
    do  { eps <- eucEPS euc ;
        ; let fam_envs = ( eps_fam_inst_env eps
                         , extendFamInstEnvList emptyFamInstEnv $ se_fam_inst opts
                         )
              simpl_env = mkSimplEnv (se_mode opts) fam_envs
              top_env_cfg = se_top_env_cfg opts
              read_eps_rules = eps_rule_base <$> eucEPS euc
              read_ruleenv = updExternalPackageRules emptyRuleEnv <$> read_eps_rules

        ; let sz = exprSize expr

        ; (expr', counts) <- initSmpl logger read_ruleenv top_env_cfg sz $
                             simplExprGently simpl_env expr

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
                  "Simplifier statistics" FormatText (pprSimplCount counts)

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl "Simplified expression"
                        FormatCore
                        (pprCoreExpr expr')

        ; return expr'
        }

simplExprGently :: SimplEnv -> CoreExpr -> SimplM CoreExpr
-- ^ Simplifies an expression by doing occurrence analysis, then simplification,
-- and repeating (twice currently), because one pass alone leaves tons of crud.
--
-- Used only:
--
--   1. for user expressions typed in at the interactive prompt (see 'GHC.Driver.Main.hscStmt'),
--   2. for Template Haskell splices (see 'GHC.Tc.Gen.Splice.runMeta').
--
-- The name 'Gently' suggests that the SimplMode is InitialPhase,
-- and in fact that is so.... but the 'Gently' in 'simplExprGently' doesn't
-- enforce that; it just simplifies the expression twice.
simplExprGently env expr = do
    expr1 <- simplExpr env (occurAnalyseExpr expr)
    simplExpr env (occurAnalyseExpr expr1)

{-
************************************************************************
*                                                                      *
\subsection{The driver for the simplifier}
*                                                                      *
************************************************************************
-}

-- | Configuration record for `simplifyPgm`.
-- The values of this datatype are /only/ driven by the demands of that function.
data SimplifyOpts = SimplifyOpts
  { so_dump_core_sizes :: !Bool
  , so_iterations      :: !Int
  , so_mode            :: !SimplMode

  , so_pass_result_cfg :: !(Maybe LintPassResultConfig)
                          -- Nothing => Do not Lint
                          -- Just cfg => Lint like this

  , so_hpt_rules       :: !RuleBase
  , so_top_env_cfg     :: !TopEnvConfig

  , so_incremental     :: !Bool
    -- ^ Use incremental (worklist-driven) simplification.
    -- When True, only re-simplify bindings whose dependencies changed.
  }

simplifyPgm :: Logger
            -> UnitEnv
            -> NamePprCtx                -- For dumping
            -> SimplifyOpts
            -> ModGuts
            -> IO (SimplCount, ModGuts)  -- New bindings

simplifyPgm logger unit_env name_ppr_ctx opts
            guts@(ModGuts { mg_module = this_mod
                          , mg_binds = binds, mg_rules = local_rules
                          , mg_fam_inst_env = fam_inst_env })
  = do { (termination_msg, it_count, counts_out, guts')
            <- if so_incremental opts
               then do_iteration_incr 1 [] binds local_rules emptyVarSet
               else do_iteration      1 [] binds local_rules

        ; when (logHasDumpFlag logger Opt_D_verbose_core2core
                && logHasDumpFlag logger Opt_D_dump_simpl_stats) $
          logDumpMsg logger
                  "Simplifier statistics for following pass"
                  (vcat [text termination_msg <+> text "after" <+> ppr it_count
                                              <+> text "iterations",
                         blankLine,
                         pprSimplCount counts_out])

        ; return (counts_out, guts')
    }
  where
    dump_core_sizes = so_dump_core_sizes opts
    mode            = so_mode opts
    max_iterations  = so_iterations opts
    top_env_cfg     = so_top_env_cfg opts
    active_rule     = activeRule mode
    active_unf      = activeUnfolding mode
    -- Note the bang in !guts_no_binds.  If you don't force `guts_no_binds`
    -- the old bindings are retained until the end of all simplifier iterations
    !guts_no_binds = guts { mg_binds = [], mg_rules = [] }

    hpt_rule_env :: RuleEnv
    hpt_rule_env = mkRuleEnv guts emptyRuleBase (so_hpt_rules opts)
                   -- emptyRuleBase: no EPS rules yet; we will update
                   -- them on each iteration to pick up the most up to date set

    -- | Prepare EPS rules, family envs, and SimplEnv for an iteration.
    --   Shared between the traditional and incremental paths.
    prep_iteration :: CoreProgram -> [CoreRule]
                   -> IO ([CoreBind], RuleEnv, IO RuleEnv, SimplEnv, Int)
    prep_iteration binds_in local_rules_in = do
      let tagged_binds = {-# SCC "OccAnal" #-}
            occurAnalysePgm this_mod active_unf active_rule
                            local_rules_in binds_in
      Logger.putDumpFileMaybe logger Opt_D_dump_occur_anal "Occurrence analysis"
                FormatCore (pprCoreBindings tagged_binds)
      eps <- ueEPS unit_env
      let !base_rule_env = updLocalRules hpt_rule_env local_rules_in
          read_eps_rules = eps_rule_base <$> ueEPS unit_env
          read_rule_env  = updExternalPackageRules base_rule_env <$> read_eps_rules
          fam_envs       = (eps_fam_inst_env eps, fam_inst_env)
          simpl_env      = mkSimplEnv mode fam_envs
          sz             = coreBindsSize binds_in
      sz `seq` return (tagged_binds, base_rule_env, read_rule_env, simpl_env, sz)

    -- | Finish an iteration: check convergence, short out indirections, dump, lint.
    finish_iteration :: Int -> [SimplCount] -> [CoreBind] -> [CoreRule] -> SimplCount
                     -> IO (String, Int, SimplCount, ModGuts)
    finish_iteration iteration_no counts_so_far binds1 rules1 counts1
      | isZeroSimplCount counts1
      = return ( "Simplifier reached fixed point", iteration_no
               , totalise (counts1 : counts_so_far)
               , guts_no_binds { mg_binds = binds1, mg_rules = rules1 } )
      | otherwise
      = do { let binds2 = {-# SCC "ZapInd" #-} shortOutIndirections binds1
           ; dump_end_iteration logger dump_core_sizes name_ppr_ctx
                                iteration_no counts1 binds2 rules1
           ; for_ (so_pass_result_cfg opts) $ \pass_result_cfg ->
               lintPassResult logger pass_result_cfg binds2
           ; return ("continue", iteration_no, counts1, guts_no_binds { mg_binds = binds2, mg_rules = rules1 })
           }

    bail_out :: Int -> [SimplCount] -> CoreProgram -> [CoreRule]
             -> IO (String, Int, SimplCount, ModGuts)
    bail_out iteration_no counts_so_far binds_cur local_rules_cur =
      warnPprTrace (debugIsOn && (max_iterations > 2))
        "Simplifier bailing out"
        ( hang (ppr this_mod <> text ", after"
                <+> int max_iterations <+> text "iterations"
                <+> (brackets $ hsep $ punctuate comma $
                     map (int . simplCountN) (reverse counts_so_far)))
             2 (text "Size =" <+> ppr (coreBindsStats binds_cur))) $
        return ( "Simplifier bailed out", iteration_no - 1
               , totalise counts_so_far
               , guts_no_binds { mg_binds = binds_cur, mg_rules = local_rules_cur } )

    totalise :: [SimplCount] -> SimplCount
    totalise = foldr (\c acc -> acc `plusSimplCount` c)
                     (zeroSimplCount $ logHasDumpFlag logger Opt_D_dump_simpl_stats)

    -----------------------------------------------------------
    -- Traditional (non-incremental) iteration loop
    -----------------------------------------------------------
    do_iteration :: Int -> [SimplCount] -> CoreProgram -> [CoreRule]
                 -> IO (String, Int, SimplCount, ModGuts)

    do_iteration iteration_no counts_so_far binds_cur local_rules_cur
      | iteration_no > max_iterations
      = bail_out iteration_no counts_so_far binds_cur local_rules_cur

      | otherwise
      = do { (tagged_binds, _, read_rule_env, simpl_env, sz)
               <- prep_iteration binds_cur local_rules_cur

           ; ((binds1, rules1), counts1) <-
               initSmpl logger read_rule_env top_env_cfg sz $
                 do { (floats, env1) <- {-# SCC "SimplTopBinds" #-}
                                        simplTopBinds simpl_env tagged_binds
                    ; rules1 <- simplImpRules env1 local_rules_cur
                    ; return (getTopFloatBinds floats, rules1) }

           ; result <- finish_iteration iteration_no counts_so_far binds1 rules1 counts1
           ; case result of
               (msg, _, _, _) | msg == "Simplifier reached fixed point"
                 -> return result
               (_, it, c1, guts') ->
                 do_iteration (it + 1) (c1 : counts_so_far)
                              (mg_binds guts') (mg_rules guts')
           }

    -----------------------------------------------------------
    -- Incremental (worklist-driven) iteration loop
    -- See Note [Incremental simplification] in Iteration.hs
    -----------------------------------------------------------
    do_iteration_incr :: Int -> [SimplCount] -> CoreProgram -> [CoreRule]
                      -> VarSet   -- ^ Binders that changed in previous iteration
                      -> IO (String, Int, SimplCount, ModGuts)

    do_iteration_incr iteration_no counts_so_far binds_cur local_rules_cur prev_changed
      | iteration_no > max_iterations
      = bail_out iteration_no counts_so_far binds_cur local_rules_cur

      | otherwise
      = do { (tagged_binds, _, read_rule_env, simpl_env, sz)
               <- prep_iteration binds_cur local_rules_cur

           -- Build the reverse dependency map from the tagged bindings
           ; let top_bndrs = mkVarSet (bindersOfBinds tagged_binds)
                 rev_deps  = buildRevDeps top_bndrs tagged_binds

           -- Compute initial dirty set for this iteration
           ; let initial_dirty
                   | iteration_no == 1
                   = top_bndrs  -- First iteration: everything is dirty
                   | otherwise
                   = -- Dirty = previously changed ∪ their dependents ∪ OccInfo-changed
                     let dep_dirty = nonDetStrictFoldVarSet
                           (\b acc -> case lookupVarEnv rev_deps b of
                                        Nothing   -> acc
                                        Just deps -> acc `unionVarSet` deps)
                           prev_changed prev_changed
                     in dep_dirty
                     -- Note: OccInfo changes are implicitly covered because
                     -- full OccAnal runs each iteration. If OccInfo changed
                     -- for a binder, its dependents will see different inline
                     -- decisions and tick, causing them to be dirty next time.

           ; dirty_ref <- newIORef initial_dirty

           ; ((binds1, rules1), counts1) <-
               initSmpl logger read_rule_env top_env_cfg sz $
                 do { (floats, env1) <- {-# SCC "SimplTopBindsIncr" #-}
                                        simplTopBindsIncr simpl_env dirty_ref
                                                          rev_deps tagged_binds
                    ; rules1 <- simplImpRules env1 local_rules_cur
                    ; return (getTopFloatBinds floats, rules1) }

           -- Determine which binders changed this iteration
           ; final_dirty <- readIORef dirty_ref
           ; let changed_this_iter = final_dirty `minusVarSet` initial_dirty
                 -- New entries added to dirty_ref during simplification
                 -- represent dependents of changed bindings.
                 -- The *actually changed* binders are those that were
                 -- dirty going in and produced ticks. We approximate this
                 -- as: anything that caused propagation.

           ; result <- finish_iteration iteration_no counts_so_far binds1 rules1 counts1
           ; case result of
               (msg, _, _, _) | msg == "Simplifier reached fixed point"
                 -> return result
               (_, it, c1, guts') ->
                 do_iteration_incr (it + 1) (c1 : counts_so_far)
                                   (mg_binds guts') (mg_rules guts')
                                   changed_this_iter
           }

-- | Build a reverse dependency map: for each top-level binder b,
--   revDeps(b) = {c | b ∈ freeVars(RHS of c)}
-- Only considers top-level binders (filters by top_bndrs set).
buildRevDeps :: VarSet -> [CoreBind] -> IdEnv VarSet
buildRevDeps top_bndrs = foldl' add_bind emptyVarEnv
  where
    add_bind env (NonRec b rhs) = add_deps env b rhs
    add_bind env (Rec pairs)    = foldl' (\e (b,rhs) -> add_deps e b rhs) env pairs

    add_deps env user rhs =
      nonDetStrictFoldVarSet add_one env dep_set
      where
        dep_set = exprFreeIds rhs `intersectVarSet` top_bndrs
        add_one dep acc = extendVarEnv_C unionVarSet acc dep (unitVarSet user)

dump_end_iteration :: Logger -> Bool -> NamePprCtx -> Int
                   -> SimplCount -> CoreProgram -> [CoreRule] -> IO ()
dump_end_iteration logger dump_core_sizes name_ppr_ctx iteration_no counts binds rules
  = dumpPassResult logger dump_core_sizes name_ppr_ctx mb_flag hdr pp_counts binds rules
  where
    mb_flag | logHasDumpFlag logger Opt_D_dump_simpl_iterations = Just Opt_D_dump_simpl_iterations
            | otherwise                                         = Nothing
            -- Show details if Opt_D_dump_simpl_iterations is on

    hdr = "Simplifier iteration=" ++ show iteration_no
    pp_counts = vcat [ text "---- Simplifier counts for" <+> text hdr
                     , pprSimplCount counts
                     , text "---- End of simplifier counts for" <+> text hdr ]

{-
************************************************************************
*                                                                      *
                Shorting out indirections
*                                                                      *
************************************************************************

If we have this:

        x_local = <expression>
        ...bindings...
        x_exported = x_local

where x_exported is exported, and x_local is not, then we replace it with this:

        x_exported = <expression>
        x_local = x_exported
        ...bindings...

Without this we never get rid of the x_exported = x_local thing.  This
save a gratuitous jump (from \tr{x_exported} to \tr{x_local}), and
makes strictness information propagate better.  This used to happen in
the final phase, but it's tidier to do it here.

Note [Messing up the exported Id's RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must be careful about discarding (obviously) or even merging the
RULES on the exported Id. The example that went bad on me at one stage
was this one:

    iterate :: (a -> a) -> a -> [a]
        [Exported]
    iterate = iterateList

    iterateFB c f x = x `c` iterateFB c f (f x)
    iterateList f x =  x : iterateList f (f x)
        [Not exported]

    {-# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterateList
     #-}

This got shorted out to:

    iterateList :: (a -> a) -> a -> [a]
    iterateList = iterate

    iterateFB c f x = x `c` iterateFB c f (f x)
    iterate f x =  x : iterate f (f x)

    {-# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterate
     #-}

And now we get an infinite loop in the rule system
        iterate f x -> build (\cn -> iterateFB c f x)
                    -> iterateFB (:) f x
                    -> iterate f x

Old "solution":
        use rule switching-off pragmas to get rid
        of iterateList in the first place

But in principle the user *might* want rules that only apply to the Id
they say.  And inline pragmas are similar
   {-# NOINLINE f #-}
   f = local
   local = <stuff>
Then we do not want to get rid of the NOINLINE.

Hence hasShortableIdinfo.


Note [Rules and indirection-zapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem: what if x_exported has a RULE that mentions something in ...bindings...?
Then the things mentioned can be out of scope!  Solution
 a) Make sure that in this pass the usage-info from x_exported is
        available for ...bindings...
 b) If there are any such RULES, rec-ify the entire top-level.
    It'll get sorted out next time round

Other remarks
~~~~~~~~~~~~~
If more than one exported thing is equal to a local thing (i.e., the
local thing really is shared), then we do one only:
\begin{verbatim}
        x_local = ....
        x_exported1 = x_local
        x_exported2 = x_local
==>
        x_exported1 = ....

        x_exported2 = x_exported1
\end{verbatim}

We rely on prior eta reduction to simplify things like
\begin{verbatim}
        x_exported = /\ tyvars -> x_local tyvars
==>
        x_exported = x_local
\end{verbatim}
Hence,there's a possibility of leaving unchanged something like this:
\begin{verbatim}
        x_local = ....
        x_exported1 = x_local Int
\end{verbatim}
By the time we've thrown away the types in STG land this
could be eliminated.  But I don't think it's very common
and it's dangerous to do this fiddling in STG land
because we might eliminate a binding that's mentioned in the
unfolding for something.

Note [Indirection zapping and ticks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unfortunately this is another place where we need a special case for
ticks. The following happens quite regularly:

        x_local = <expression>
        x_exported = tick<x> x_local

Which we want to become:

        x_exported =  tick<x> <expression>

As it makes no sense to keep the tick and the expression on separate
bindings. Note however that this might increase the ticks scoping
over the execution of x_local, so we can only do this for floatable
ticks. More often than not, other references will be unfoldings of
x_exported, and therefore carry the tick anyway.
-}

type IndEnv = IdEnv (Id, [CoreTickish]) -- Maps local_id -> exported_id, ticks

shortOutIndirections :: CoreProgram -> CoreProgram
shortOutIndirections binds
  | isEmptyVarEnv ind_env = binds
  | no_need_to_flatten    = binds'                      -- See Note [Rules and indirection-zapping]
  | otherwise             = [Rec (flattenBinds binds')] -- for this no_need_to_flatten stuff
  where
    ind_env            = makeIndEnv binds
    -- These exported Ids are the subjects  of the indirection-elimination
    exp_ids            = map fst $ nonDetEltsUFM ind_env
      -- It's OK to use nonDetEltsUFM here because we forget the ordering
      -- by immediately converting to a set or check if all the elements
      -- satisfy a predicate.
    exp_id_set         = mkVarSet exp_ids
    no_need_to_flatten = all (null . ruleInfoRules . idSpecialisation) exp_ids
    binds'             = concatMap zap binds

    zap (NonRec bndr rhs) = [NonRec b r | (b,r) <- zapPair (bndr,rhs)]
    zap (Rec pairs)       = [Rec (concatMap zapPair pairs)]

    zapPair (bndr, rhs)
        | bndr `elemVarSet` exp_id_set
        = []   -- Kill the exported-id binding

        | Just (exp_id, ticks) <- lookupVarEnv ind_env bndr
        , (exp_id', lcl_id') <- transferIdInfo exp_id bndr
        =      -- Turn a local-id binding into two bindings
               --    exp_id = rhs; lcl_id = exp_id
          [ (exp_id', mkTicks ticks rhs),
            (lcl_id', Var exp_id') ]

        | otherwise
        = [(bndr,rhs)]

makeIndEnv :: [CoreBind] -> IndEnv
makeIndEnv binds
  = foldl' add_bind emptyVarEnv binds
  where
    add_bind :: IndEnv -> CoreBind -> IndEnv
    add_bind env (NonRec exported_id rhs) = add_pair env (exported_id, rhs)
    add_bind env (Rec pairs)              = foldl' add_pair env pairs

    add_pair :: IndEnv -> (Id,CoreExpr) -> IndEnv
    add_pair env (exported_id, exported)
        | (ticks, Var local_id) <- stripTicksTop tickishFloatable exported
        , shortMeOut env exported_id local_id
        = extendVarEnv env local_id (exported_id, ticks)
    add_pair env _ = env

shortMeOut :: IndEnv -> Id -> Id -> Bool
shortMeOut ind_env exported_id local_id
-- The if-then-else stuff is just so I can get a pprTrace to see
-- how often I don't get shorting out because of IdInfo stuff
  = if isExportedId exported_id &&              -- Only if this is exported

       isLocalId local_id &&                    -- Only if this one is defined in this
                                                --      module, so that we *can* change its
                                                --      binding to be the exported thing!

       not (isExportedId local_id) &&           -- Only if this one is not itself exported,
                                                --      since the transformation will nuke it

       not (local_id `elemVarEnv` ind_env)      -- Only if not already substituted for
    then
        if hasShortableIdInfo exported_id
        then True       -- See Note [Messing up the exported Id's RULES]
        else warnPprTrace True "Not shorting out" (ppr exported_id) False
    else
        False

hasShortableIdInfo :: Id -> Bool
-- True if there is no user-attached IdInfo on exported_id,
-- so we can safely discard it
-- See Note [Messing up the exported Id's RULES]
hasShortableIdInfo id
  =  isEmptyRuleInfo (ruleInfo info)
  && isDefaultInlinePragma (inlinePragInfo info)
  && not (isStableUnfolding (realUnfoldingInfo info))
  where
     info = idInfo id

{- Note [Transferring IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
     lcl_id = e; exp_id = lcl_id

and lcl_id has useful IdInfo, we don't want to discard it by going
     gbl_id = e; lcl_id = gbl_id

Instead, transfer IdInfo from lcl_id to exp_id, specifically
* (Stable) unfolding
* Strictness
* Rules
* Inline pragma

Overwriting, rather than merging, seems to work ok.

For the lcl_id we

* Zap the InlinePragma. It might originally have had a NOINLINE, which
  we have now transferred; and we really want the lcl_id to inline now
  that its RHS is trivial!

* Zap any Stable unfolding.  agian, we want lcl_id = gbl_id to inline,
  replacing lcl_id by gbl_id. That won't happen if lcl_id has its original
  great big Stable unfolding
-}

transferIdInfo :: Id -> Id -> (Id, Id)
-- See Note [Transferring IdInfo]
transferIdInfo exported_id local_id
  = ( modifyIdInfo transfer exported_id
    , modifyIdInfo zap_info local_id )
  where
    local_info = idInfo local_id
    transfer exp_info = exp_info `setDmdSigInfo`     dmdSigInfo local_info
                                 `setCprSigInfo`     cprSigInfo local_info
                                 `setUnfoldingInfo`  realUnfoldingInfo local_info
                                 `setInlinePragInfo` inlinePragInfo local_info
                                 `setRuleInfo`       addRuleInfo (ruleInfo exp_info) new_info
    new_info = setRuleInfoHead (idName exported_id)
                               (ruleInfo local_info)
        -- Remember to set the function-name field of the
        -- rules as we transfer them from one function to another

    zap_info lcl_info = lcl_info `setInlinePragInfo` defaultInlinePragma
                                 `setUnfoldingInfo`  noUnfolding
