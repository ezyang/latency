{-# LANGUAGE ViewPatterns #-}
import Foreign.C.Types
import Foreign.C.Error
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Concurrent
import Control.Monad
import System.Exit
import System.Posix.Process
import System.IO
import Data.Time.Clock.POSIX
import System.Environment
import Text.Printf

import System.Posix.Internals
import GHC.Event

afUnix = 1
sockDgram = 2

socketpair = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \fdArr -> do
    throwErrnoIfMinus1Retry_ "socketpair" $ c_socketpair afUnix sockDgram 0 fdArr
    [fd1, fd2] <- peekArray 2 fdArr
    setNonBlockingFD fd1 True
    setNonBlockingFD fd2 True
    return (fd1, fd2)

socketFork f = do
    (fd1, fd2) <- socketpair
    _ <- forkProcess $ do
        c_close fd1
        f fd2
    c_close fd2
    return fd1


thread :: MVar Int -> MVar Int -> IO ()
thread inp out = do x <- takeMVar inp; putMVar out $! x+1; thread inp out

spawn cur n = do next <- newEmptyMVar
                 forkIO $ thread cur next
                 return next

n2 = 100000000

main = do
    n <- readIO . head =<< getArgs
    fd <- socketFork $ \fd ->
        -- process being benchmarked
        -- XXX need to do other random stuff in the background
        allocaBytes 1 $ \recvBuf ->
        withCAStringLen "2" $ \(sendBuf, fromIntegral -> len) -> do
        {-
        forkIO $ do
          s <- newEmptyMVar
          e <- foldM spawn s [1..500]
          f <- newEmptyMVar
          forkIO $ replicateM n2 (takeMVar e) >>= putMVar f . sum
          replicateM n2 (forkIO $ putMVar s 0)
          takeMVar f
          return ()
          -}
        forever $ do
            threadWaitRead (fromIntegral fd)
            c_recv fd recvBuf 1 0
            c_send fd sendBuf len 0
            r <- peek recvBuf
            when (castCCharToChar r == '0') $ exitSuccess
    allocaBytes 1 $ \recvBuf -> do
    withCAStringLen "1" $ \(sendBuf, fromIntegral -> len) -> do
    replicateM_ n $ do
        a <- getTime
        c_send fd sendBuf (fromIntegral len) 0
        threadWaitRead (fromIntegral fd)
        c_recv fd recvBuf 1 0
        b <- getTime
        return ()
        hPrintf stdout "%.6f\n" (b - a)
    withCAStringLen "0" $ \(sendBuf, len) -> do
    c_send fd sendBuf (fromIntegral len) 0

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

-- XXX problem, this is Unix only
foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
