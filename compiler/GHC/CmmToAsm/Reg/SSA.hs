{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Dominator-tree-preorder greedy register allocator.
--
-- Builds an interference graph from liveness annotations, then
-- greedily colors virtual registers processing definitions in
-- dominator-tree preorder — a heuristic that produces good colorings
-- for the chordal-like interference graphs common in compiled code.
--
-- See Note [SSA-based register allocation] at the end of this module.

module GHC.CmmToAsm.Reg.SSA (regAlloc) where

import GHC.Prelude

import GHC.CmmToAsm.Reg.Liveness
import GHC.CmmToAsm.Reg.Graph.Spill ( regSpill )
import GHC.CmmToAsm.Reg.Graph.Stats ( RegAllocStats )
import GHC.CmmToAsm.Reg.Target ( targetClassOfReg )
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Format ( RegWithFormat(..) )
import GHC.CmmToAsm.Reg.Regs ( Regs, getRegs )
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.CFG ( CFG )
import qualified GHC.CmmToAsm.CFG.Dominators as Dom

import GHC.Platform
import GHC.Platform.Reg
import GHC.Platform.Reg.Class ( RegClass )

import GHC.Cmm ( GenCmmDecl(..) )
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow.Label

import GHC.Types.Unique ( Uniquable(..), getKey )
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Unique.DSM ( UniqDSM )

import GHC.Data.Bag ( Bag, bagToList )
import GHC.Data.Graph.Directed ( flattenSCCs )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc ( HasDebugCallStack )

import Data.Tree ( Tree(..), flatten )
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import Data.Word ( Word64 )
import qualified GHC.Data.Word64Map as WM

import Control.Monad ( foldM )

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

maxSpinCount :: Int
maxSpinCount = 10

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

regAlloc
  :: forall instr statics.
     (OutputableP Platform statics, Instruction instr, HasDebugCallStack)
  => NCGConfig
  -> UniqFM RegClass (UniqSet RealReg)
  -> UniqSet Int
  -> Int
  -> [LiveCmmDecl statics instr]
  -> Maybe CFG
  -> UniqDSM ( [NatCmmDecl statics instr]
             , Maybe Int
             , [RegAllocStats statics instr] )

regAlloc config regsFree _slotsFree slotsCount code _cfg = do
  let platform = ncgPlatform config
  (natCode, finalSlots) <- foldM (allocOne config platform regsFree)
                                 ([], slotsCount) code
  let needStack = if slotsCount == finalSlots then Nothing else Just finalSlots
  return (reverse natCode, needStack, [])

allocOne
  :: forall instr statics.
     (OutputableP Platform statics, Instruction instr, HasDebugCallStack)
  => NCGConfig -> Platform
  -> UniqFM RegClass (UniqSet RealReg)
  -> ([NatCmmDecl statics instr], Int)
  -> LiveCmmDecl statics instr
  -> UniqDSM ([NatCmmDecl statics instr], Int)
allocOne _ _ _ (acc, slots) (CmmData sec ds) =
  return (CmmData sec ds : acc, slots)
allocOne config platform regsFree (acc, slots) liveProc = do
  (nat, slots') <- ssaProcSpin config platform regsFree
                               (mkUniqSet [0 .. slots - 1]) slots 0 liveProc
  return (nat : acc, slots')

------------------------------------------------------------------------
-- Per-procedure spill iteration
------------------------------------------------------------------------

ssaProcSpin
  :: forall instr statics.
     (OutputableP Platform statics, Instruction instr, HasDebugCallStack)
  => NCGConfig -> Platform
  -> UniqFM RegClass (UniqSet RealReg)
  -> UniqSet Int -> Int -> Int
  -> LiveCmmDecl statics instr
  -> UniqDSM (NatCmmDecl statics instr, Int)

ssaProcSpin _ _ _ _ slots _ (CmmData sec ds) =
  return (CmmData sec ds, slots)

ssaProcSpin config platform regsFree slotsFree slotsCount spinCount
  proc@(CmmProc (LiveInfo info (entry:_) _blockLive _) label live sccs)
  | spinCount > maxSpinCount
  = pprPanic "SSA regAlloc: too many spill iterations" (int spinCount)
  | otherwise
  = do
    let allBlocks = flattenSCCs sccs
    let result = ssaAllocProc config platform regsFree entry proc allBlocks
    case result of
      Right natBlocks ->
        return (CmmProc info label live (ListGraph natBlocks), slotsCount)
      Left spillSet -> do
        (code_spilled, slotsFree', slotsCount', _) <-
          regSpill platform [proc] slotsFree slotsCount spillSet
        code_relive <- mapM (regLiveness platform . reverseBlocksInTops)
                            code_spilled
        case code_relive of
          [proc'] -> ssaProcSpin config platform regsFree slotsFree'
                                slotsCount' (spinCount + 1) proc'
          _ -> pprPanic "SSA regAlloc: unexpected proc count after spill" empty

ssaProcSpin _ _ _ _ _ _ _ =
  pprPanic "SSA regAlloc: proc without entry block" empty

------------------------------------------------------------------------
-- Core allocation: interference graph + domtree-preorder greedy coloring
------------------------------------------------------------------------

ssaAllocProc
  :: forall instr statics.
     (Instruction instr, HasDebugCallStack, OutputableP Platform statics)
  => NCGConfig -> Platform
  -> UniqFM RegClass (UniqSet RealReg)
  -> BlockId
  -> LiveCmmDecl statics instr
  -> [LiveBasicBlock instr]
  -> Either (UniqSet VirtualReg) [NatBasicBlock instr]

ssaAllocProc config platform regsFree entry liveProc liveBlocks =
  case colorResult of
    Left spillVRs -> Left spillVRs
    Right coloring ->
      let patchReg :: Reg -> Reg
          patchReg (RegVirtual vr) = case lookupUFM coloring vr of
            Just rr -> RegReal rr
            Nothing -> RegVirtual vr
          patchReg r = r
          natBlocks =
            [ let raw = concatMap (liveInstrToNat config platform patchReg) instrs
              in BasicBlock bid raw
            | BasicBlock bid instrs <- liveBlocks ]
      in Right natBlocks
  where
    -- Step 1: Build interference graph from slurpConflicts
    (conflictBag, _movesBag) = slurpConflicts platform liveProc
    interferenceGraph = buildInterferenceGraph platform conflictBag

    -- Step 2: Compute dominator tree and get definition order
    succMap :: LabelMap [BlockId]
    succMap = mapFromList
      [ (bid, concatMap getJumpDests instrs)
      | BasicBlock bid instrs <- liveBlocks ]

    getJumpDests :: LiveInstr instr -> [BlockId]
    getJumpDests (LiveInstr (Instr i) _)
      | isJumpishInstr i = jumpDestsOfInstr i
    getJumpDests _ = []

    allBlockIds = [bid | BasicBlock bid _ <- liveBlocks]
    domTreeRoot = computeDomTree entry succMap allBlockIds
    domPreorder = flatten domTreeRoot

    -- Step 3: Collect vreg definitions in domtree preorder
    blockMap :: LabelMap (LiveBasicBlock instr)
    blockMap = mapFromList [(bid, b) | b@(BasicBlock bid _) <- liveBlocks]

    -- All vregs mentioned in any instruction (reads + writes),
    -- in domtree preorder of their first occurrence block, then
    -- remaining blocks not in the domtree (unreachable from entry).
    -- We include both reads and writes because function parameters
    -- may be read but never written within this procedure.
    blockOrder :: [BlockId]
    blockOrder =
      let domSet = foldl' (flip setInsert) setEmpty domPreorder
          remaining = filter (\b -> not (setMember b domSet)) allBlockIds
      in domPreorder ++ remaining

    defOrder :: [VirtualReg]
    defOrder = concatMap blockVRegs blockOrder
      where
        blockVRegs bid = case mapLookup bid blockMap of
          Nothing -> []
          Just (BasicBlock _ instrs) ->
            [ vr
            | LiveInstr instr _ <- instrs
            , let RU reads writes = regUsageOfInstr platform instr
            , RegWithFormat (RegVirtual vr) _ <- reads ++ writes
            ]

    -- Deduplicate while preserving first-occurrence order
    defOrderDeduped :: [VirtualReg]
    defOrderDeduped = go emptyUniqSet defOrder
      where
        go _ [] = []
        go seen (vr:vrs)
          | elementOfUniqSet vr seen = go seen vrs
          | otherwise = vr : go (addOneToUniqSet seen vr) vrs

    -- Step 4: Greedy color in domtree preorder
    colorResult = greedyColorOrder platform regsFree interferenceGraph defOrderDeduped

------------------------------------------------------------------------
-- Interference graph construction
------------------------------------------------------------------------

-- | Interference data for each VirtualReg: interfering VirtualRegs and RealRegs.
data Interference = Interference
  { ifVRegs :: !(UniqSet VirtualReg)
  , ifRRegs :: !(UniqSet RealReg)
  }

emptyInterference :: Interference
emptyInterference = Interference emptyUniqSet emptyUniqSet

type InterferenceGraph = UniqFM VirtualReg Interference

buildInterferenceGraph
  :: Platform
  -> Bag Regs
  -> InterferenceGraph
buildInterferenceGraph _platform conflictBag =
  foldl' addClique emptyUFM (bagToList conflictBag)
  where
    addClique :: InterferenceGraph -> Regs -> InterferenceGraph
    addClique graph regs =
      let rwfs = nonDetEltsUniqSet (getRegs regs)
          vregs = [ vr  | RegWithFormat (RegVirtual vr)  _ <- rwfs ]
          rregs = [ rr  | RegWithFormat (RegReal rr)     _ <- rwfs ]
          vregSet = mkUniqSet vregs
          rregSet = mkUniqSet rregs
      in foldl' (\g vr ->
           let otherVRegs = delOneFromUniqSet vregSet vr
               existing = fromMaybe emptyInterference (lookupUFM g vr)
               updated = Interference
                 { ifVRegs = ifVRegs existing `unionUniqSets` otherVRegs
                 , ifRRegs = ifRRegs existing `unionUniqSets` rregSet
                 }
           in addToUFM g vr updated
         ) graph vregs

------------------------------------------------------------------------
-- Greedy coloring
------------------------------------------------------------------------

greedyColorOrder
  :: Platform
  -> UniqFM RegClass (UniqSet RealReg)
  -> InterferenceGraph
  -> [VirtualReg]
  -> Either (UniqSet VirtualReg) (UniqFM VirtualReg RealReg)
greedyColorOrder platform regsFree ifGraph vregs =
  foldl' colorOne (Right emptyUFM) vregs
  where
    colorOne (Left spills) _ = Left spills
    colorOne (Right coloring) vr =
      let cls = targetClassOfReg platform (RegVirtual vr)
          intf = fromMaybe emptyInterference (lookupUFM ifGraph vr)
          -- Real regs occupied by interfering vregs (same class)
          occupiedByVRegs = foldl' (\acc neighbor ->
            case lookupUFM coloring neighbor of
              Just rr | targetClassOfReg platform (RegReal rr) == cls
                      -> addOneToUniqSet acc rr
              _       -> acc
            ) emptyUniqSet (nonDetEltsUniqSet (ifVRegs intf))
          -- Real regs directly interfered with (fixed registers)
          occupiedByFixed = filterUniqSet
            (\rr -> targetClassOfReg platform (RegReal rr) == cls)
            (ifRRegs intf)
          occupied = occupiedByVRegs `unionUniqSets` occupiedByFixed
          allRegs = fromMaybe emptyUniqSet (lookupUFM regsFree cls)
          available = allRegs `minusUniqSet` occupied
      in case pickReg available of
           Nothing -> Left (unitUniqSet vr)
           Just r  -> Right (addToUFM coloring vr r)

pickReg :: UniqSet RealReg -> Maybe RealReg
pickReg s = case nonDetEltsUniqSet s of
  []    -> Nothing
  (r:_) -> Just r

------------------------------------------------------------------------
-- Convert LiveInstr to native instructions with register patching
------------------------------------------------------------------------

liveInstrToNat
  :: (Instruction instr, HasDebugCallStack)
  => NCGConfig -> Platform -> (Reg -> Reg)
  -> LiveInstr instr -> [instr]
liveInstrToNat _config _platform _patchReg (LiveInstr (Instr i) _)
  | isMetaInstr i = []
liveInstrToNat _config platform patchReg (LiveInstr (Instr i) _) =
  [patchRegsOfInstr platform i patchReg]
liveInstrToNat config _platform patchReg (LiveInstr (SPILL reg slot) _) =
  mkSpillInstr config (patchRWF patchReg reg) 0 slot
liveInstrToNat config _platform patchReg (LiveInstr (RELOAD slot reg) _) =
  mkLoadInstr config (patchRWF patchReg reg) 0 slot

patchRWF :: (Reg -> Reg) -> RegWithFormat -> RegWithFormat
patchRWF f (RegWithFormat r fmt) = RegWithFormat (f r) fmt

------------------------------------------------------------------------
-- Dominator computation
------------------------------------------------------------------------

blockToW64 :: BlockId -> Word64
blockToW64 = getKey . getUnique

computeDomTree
  :: BlockId -> LabelMap [BlockId] -> [BlockId]
  -> Tree BlockId
computeDomTree entry succMap allBlocks =
  convertTree w64Map dTree
  where
    w64Map = WM.fromList [(blockToW64 b, b) | b <- allBlocks]
    adj = [ (blockToW64 bid, map blockToW64 succs)
          | (bid, succs) <- mapToList succMap ]
    graph = Dom.fromAdj adj
    rooted = (blockToW64 entry, graph)
    dTree = Dom.domTree rooted

convertTree :: WM.Word64Map BlockId -> Tree Word64 -> Tree BlockId
convertTree m (Node w cs) = Node (lookupW64 m w) (map (convertTree m) cs)

lookupW64 :: WM.Word64Map BlockId -> Word64 -> BlockId
lookupW64 m w = fromMaybe (pprPanic "SSA: unknown block" (ppr w)) (WM.lookup w m)

------------------------------------------------------------------------
-- Note [SSA-based register allocation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This allocator uses dominator-tree preorder to guide greedy register
-- coloring. The key idea from Hack, Grund, and Goos (CC 2006) is that
-- processing definitions in dominator-tree preorder provides an optimal
-- coloring for chordal interference graphs (which arise naturally under
-- SSA form).
--
-- The pipeline:
--   1. Build interference graph from liveness annotations
--      (using slurpConflicts, which extracts conflict cliques from
--      the LiveInstr annotations computed by regLiveness)
--   2. Compute dominator tree (Lengauer-Tarjan, from CFG.Dominators)
--   3. Collect vreg definitions in dominator-tree preorder
--   4. Greedily color vregs in that order, checking interference
--      neighbors for already-assigned colors
--   5. Patch registers and lower SPILL/RELOAD to machine instructions
--   6. On color failure: use regSpill infrastructure, retry
--
-- The flag -fregs-ssa enables this allocator instead of the default
-- linear scan or graph coloring allocators.
------------------------------------------------------------------------
