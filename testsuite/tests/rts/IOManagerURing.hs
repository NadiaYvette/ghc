{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}

-- | Test that exercises io_uring I/O paths with intermixed file and pipe
-- I/O happening concurrently across multiple threads.
--
-- This test verifies data integrity: each channel transfers a known byte
-- sequence and we check it arrives correctly. It is designed to stress
-- the interleaving of file I/O (which goes through io_uring's
-- IORING_OP_READ/WRITE in both threaded and non-threaded RTS) and pipe
-- I/O (which uses readiness notification or blocking safe FFI).

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Word
import Foreign
import Foreign.C
import System.IO
import System.Posix.IO (createPipe, fdToHandle, closeFd)
import System.Posix.Types (Fd(..))

main :: IO ()
main = do
    putStrLn "io_uring mixed I/O tests"

    putStr "1. File I/O (sequential)... "
    testFileIO
    putStrLn "ok"

    putStr "2. Pipe I/O (sequential)... "
    testPipeIO
    putStrLn "ok"

    putStr "3. Bidirectional socket I/O... "
    testSocketPairIO
    putStrLn "ok"

    putStr "4. Concurrent file + pipe... "
    testConcurrentFilePipe
    putStrLn "ok"

    putStr "5. Concurrent file + socket... "
    testConcurrentFileSocket
    putStrLn "ok"

    putStr "6. Concurrent file + pipe + socket (stress)... "
    testConcurrentAll
    putStrLn "ok"

    putStr "7. Many concurrent file writers/readers... "
    testManyFileThreads
    putStrLn "ok"

    putStr "8. Large file I/O... "
    testLargeFileIO
    putStrLn "ok"

    putStrLn "All tests passed."

------------------------------------------------------------------------
-- Test data generation and verification
------------------------------------------------------------------------

-- | Generate a deterministic byte pattern: byte i = i mod 251
-- (251 is prime, so the pattern doesn't repeat at power-of-2 boundaries)
generateData :: Ptr Word8 -> Int -> IO ()
generateData ptr n =
    forM_ [0..n-1] $ \i ->
        pokeByteOff ptr i (fromIntegral (i `mod` 251) :: Word8)

-- | Verify data matches the expected pattern
verifyData :: String -> Ptr Word8 -> Int -> IO ()
verifyData label ptr n =
    forM_ [0..n-1] $ \i -> do
        actual <- peekByteOff ptr i :: IO Word8
        let expected = fromIntegral (i `mod` 251)
        when (actual /= expected) $
            error $ label ++ ": byte " ++ show i ++ " mismatch: expected "
                  ++ show expected ++ " got " ++ show actual

------------------------------------------------------------------------
-- Temp file helpers
------------------------------------------------------------------------

foreign import capi "stdlib.h mkstemp" c_mkstemp :: CString -> IO CInt
foreign import capi "unistd.h unlink" c_unlink :: CString -> IO CInt

-- | Create a temp file, run the action, then clean up.
withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile action = do
    let template = "/tmp/uring-test-XXXXXX"
    allocaBytes (length template + 1) $ \cstr -> do
        pokeArray0 0 cstr (map (fromIntegral . fromEnum) template)
        fd <- c_mkstemp cstr
        when (fd == -1) $ error "mkstemp failed"
        path <- peekCString cstr
        h <- fdToHandle (Fd fd)
        hSetBinaryMode h True
        (action path h `finally` (hClose h >> c_unlink cstr >> return ()))

------------------------------------------------------------------------
-- Socket pair helper (via C FFI, like IOManager.hsc)
------------------------------------------------------------------------

foreign import capi "sys/socket.h socketpair"
    c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

-- AF_UNIX = 1, SOCK_STREAM = 1 on Linux
withSocketPair :: (Handle -> Handle -> IO a) -> IO a
withSocketPair action =
    allocaBytes (2 * sizeOf (undefined :: CInt)) $ \fds -> do
        rc <- c_socketpair 1 1 0 fds  -- AF_UNIX, SOCK_STREAM
        when (rc /= 0) $ error "socketpair failed"
        fd0 <- peekElemOff fds 0
        fd1 <- peekElemOff fds 1
        h0 <- fdToHandle (Fd fd0)
        h1 <- fdToHandle (Fd fd1)
        hSetBinaryMode h0 True
        hSetBinaryMode h1 True
        hSetBuffering h0 NoBuffering
        hSetBuffering h1 NoBuffering
        action h0 h1 `finally` (hClose h0 >> hClose h1)

------------------------------------------------------------------------
-- Pipe helper
------------------------------------------------------------------------

withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe action = do
    (readEnd, writeEnd) <- createPipe
    rh <- fdToHandle readEnd
    wh <- fdToHandle writeEnd
    hSetBinaryMode rh True
    hSetBinaryMode wh True
    hSetBuffering rh NoBuffering
    hSetBuffering wh NoBuffering
    action rh wh `finally` (hClose rh >> hClose wh)

------------------------------------------------------------------------
-- Read all bytes from a handle into a buffer
------------------------------------------------------------------------

readAllInto :: Handle -> Ptr Word8 -> Int -> IO Int
readAllInto h buf total = go 0
  where
    go !off
      | off >= total = return off
      | otherwise = do
          n <- hGetBuf h (buf `plusPtr` off) (total - off)
          if n == 0
            then return off  -- EOF
            else go (off + n)

------------------------------------------------------------------------
-- Test 1: File I/O
------------------------------------------------------------------------

testFileIO :: IO ()
testFileIO = do
    let sz = 100000
    allocaBytes sz $ \wbuf ->
      allocaBytes sz $ \rbuf -> do
        generateData wbuf sz
        withTempFile $ \path h -> do
            hPutBuf h wbuf sz
            hSeek h AbsoluteSeek 0
            n <- readAllInto h rbuf sz
            when (n /= sz) $ error $ "fileIO: short read " ++ show n
            verifyData "fileIO" rbuf sz

------------------------------------------------------------------------
-- Test 2: Pipe I/O
------------------------------------------------------------------------

testPipeIO :: IO ()
testPipeIO = do
    let sz = 50000
    allocaBytes sz $ \wbuf ->
      allocaBytes sz $ \rbuf -> do
        generateData wbuf sz
        withPipe $ \rh wh -> do
            -- Writer thread (pipe write can block if buffer fills)
            _ <- forkIO $ do
                hPutBuf wh wbuf sz
                hClose wh
            n <- readAllInto rh rbuf sz
            when (n /= sz) $ error $ "pipeIO: short read " ++ show n
            verifyData "pipeIO" rbuf sz

------------------------------------------------------------------------
-- Test 3: Bidirectional socket I/O
------------------------------------------------------------------------

testSocketPairIO :: IO ()
testSocketPairIO = do
    let sz = 50000
    allocaBytes sz $ \wbuf ->
      allocaBytes sz $ \rbuf -> do
        generateData wbuf sz
        withSocketPair $ \h0 h1 -> do
            _ <- forkIO $ do
                hPutBuf h0 wbuf sz
                hClose h0
            n <- readAllInto h1 rbuf sz
            when (n /= sz) $ error $ "socketIO: short read " ++ show n
            verifyData "socketIO" rbuf sz

------------------------------------------------------------------------
-- Test 4: Concurrent file + pipe
------------------------------------------------------------------------

testConcurrentFilePipe :: IO ()
testConcurrentFilePipe = do
    let fileSz = 100000
        pipeSz = 50000

    fileDone <- newEmptyMVar
    pipeDone <- newEmptyMVar

    _ <- forkIO $ do
        allocaBytes fileSz $ \wbuf ->
          allocaBytes fileSz $ \rbuf -> do
            generateData wbuf fileSz
            withTempFile $ \_ h -> do
                hPutBuf h wbuf fileSz
                hSeek h AbsoluteSeek 0
                n <- readAllInto h rbuf fileSz
                when (n /= fileSz) $ error "concFP/file: short read"
                verifyData "concFP/file" rbuf fileSz
        putMVar fileDone ()

    _ <- forkIO $ do
        allocaBytes pipeSz $ \wbuf ->
          allocaBytes pipeSz $ \rbuf -> do
            generateData wbuf pipeSz
            withPipe $ \rh wh -> do
                _ <- forkIO $ hPutBuf wh wbuf pipeSz >> hClose wh
                n <- readAllInto rh rbuf pipeSz
                when (n /= pipeSz) $ error "concFP/pipe: short read"
                verifyData "concFP/pipe" rbuf pipeSz
        putMVar pipeDone ()

    takeMVar fileDone
    takeMVar pipeDone

------------------------------------------------------------------------
-- Test 5: Concurrent file + socket
------------------------------------------------------------------------

testConcurrentFileSocket :: IO ()
testConcurrentFileSocket = do
    let fileSz = 100000
        sockSz = 50000

    fileDone <- newEmptyMVar
    sockDone <- newEmptyMVar

    _ <- forkIO $ do
        allocaBytes fileSz $ \wbuf ->
          allocaBytes fileSz $ \rbuf -> do
            generateData wbuf fileSz
            withTempFile $ \_ h -> do
                hPutBuf h wbuf fileSz
                hSeek h AbsoluteSeek 0
                n <- readAllInto h rbuf fileSz
                when (n /= fileSz) $ error "concFS/file: short read"
                verifyData "concFS/file" rbuf fileSz
        putMVar fileDone ()

    _ <- forkIO $ do
        allocaBytes sockSz $ \wbuf ->
          allocaBytes sockSz $ \rbuf -> do
            generateData wbuf sockSz
            withSocketPair $ \h0 h1 -> do
                _ <- forkIO $ hPutBuf h0 wbuf sockSz >> hClose h0
                n <- readAllInto h1 rbuf sockSz
                when (n /= sockSz) $ error "concFS/sock: short read"
                verifyData "concFS/sock" rbuf sockSz
        putMVar sockDone ()

    takeMVar fileDone
    takeMVar sockDone

------------------------------------------------------------------------
-- Test 6: Concurrent file + pipe + socket (stress)
------------------------------------------------------------------------

testConcurrentAll :: IO ()
testConcurrentAll = do
    let nRounds = 5
    counter <- newIORef (0 :: Int)
    dones <- replicateM (nRounds * 3) newEmptyMVar

    forM_ (zip [0..nRounds-1] (chunksOf 3 dones)) $ \(i, mvars) -> do
        let [fDone, pDone, sDone] = mvars
            fileSz = 50000 + i * 10000
            pipeSz = 10000 + i * 5000
            sockSz = 10000 + i * 7000

        -- File thread
        _ <- forkIO $ do
            allocaBytes fileSz $ \wbuf ->
              allocaBytes fileSz $ \rbuf -> do
                generateData wbuf fileSz
                withTempFile $ \_ h -> do
                    hPutBuf h wbuf fileSz
                    hSeek h AbsoluteSeek 0
                    n <- readAllInto h rbuf fileSz
                    when (n /= fileSz) $ error $ "stress/file/" ++ show i
                    verifyData ("stress/file/" ++ show i) rbuf fileSz
            atomicModifyIORef' counter (\n -> (n+1, ()))
            putMVar fDone ()

        -- Pipe thread
        _ <- forkIO $ do
            allocaBytes pipeSz $ \wbuf ->
              allocaBytes pipeSz $ \rbuf -> do
                generateData wbuf pipeSz
                withPipe $ \rh wh -> do
                    _ <- forkIO $ hPutBuf wh wbuf pipeSz >> hClose wh
                    n <- readAllInto rh rbuf pipeSz
                    when (n /= pipeSz) $ error $ "stress/pipe/" ++ show i
                    verifyData ("stress/pipe/" ++ show i) rbuf pipeSz
            atomicModifyIORef' counter (\n -> (n+1, ()))
            putMVar pDone ()

        -- Socket thread
        _ <- forkIO $ do
            allocaBytes sockSz $ \wbuf ->
              allocaBytes sockSz $ \rbuf -> do
                generateData wbuf sockSz
                withSocketPair $ \h0 h1 -> do
                    _ <- forkIO $ hPutBuf h0 wbuf sockSz >> hClose h0
                    n <- readAllInto h1 rbuf sockSz
                    when (n /= sockSz) $ error $ "stress/sock/" ++ show i
                    verifyData ("stress/sock/" ++ show i) rbuf sockSz
            atomicModifyIORef' counter (\n -> (n+1, ()))
            putMVar sDone ()

        return ()

    mapM_ takeMVar dones
    n <- readIORef counter
    when (n /= nRounds * 3) $
        error $ "stress: expected " ++ show (nRounds * 3)
              ++ " completions, got " ++ show n

------------------------------------------------------------------------
-- Test 7: Many concurrent file writers/readers
------------------------------------------------------------------------

testManyFileThreads :: IO ()
testManyFileThreads = do
    let nThreads = 20
        sz      = 50000
    dones <- replicateM nThreads newEmptyMVar

    forM_ (zip [0..] dones) $ \(i, done) ->
        forkIO $ do
            allocaBytes sz $ \wbuf ->
              allocaBytes sz $ \rbuf -> do
                generateData wbuf sz
                withTempFile $ \_ h -> do
                    hPutBuf h wbuf sz
                    hSeek h AbsoluteSeek 0
                    n <- readAllInto h rbuf sz
                    when (n /= sz) $ error $ "manyFile/" ++ show (i :: Int)
                    verifyData ("manyFile/" ++ show i) rbuf sz
            putMVar done ()

    mapM_ takeMVar dones

------------------------------------------------------------------------
-- Test 8: Large file I/O (1MB)
------------------------------------------------------------------------

testLargeFileIO :: IO ()
testLargeFileIO = do
    let sz = 1024 * 1024  -- 1 MB
    allocaBytes sz $ \wbuf ->
      allocaBytes sz $ \rbuf -> do
        generateData wbuf sz
        withTempFile $ \_ h -> do
            hPutBuf h wbuf sz
            hSeek h AbsoluteSeek 0
            n <- readAllInto h rbuf sz
            when (n /= sz) $ error $ "largeFile: short read " ++ show n
            verifyData "largeFile" rbuf sz

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
