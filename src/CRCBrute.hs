module CRCBrute
       ( crcBruteUtf8
       ) where

import Control.Monad
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (copyBytes, fromBool)
import Foreign.Storable
import Data.IORef
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Concurrent.Async.Pool
import System.CPUTime
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as VS

import Utils

type Periodic = CULLong -> IO CInt
type Check = IO CInt

newtype {-# CTYPE "utf8_c" #-} CUTF8Char = CUTF8Char { unCUTF8Char :: Char }
                                         deriving (Show, Eq)

instance Storable CUTF8Char where
  sizeOf _ = 4

  alignment _ = alignment (undefined :: CInt)

  poke p c'@(CUTF8Char c) = B.unsafeUseAsCStringLen (T.encodeUtf8 $ T.singleton c) $ \(cptr, l) -> do
    copyBytes (castPtr p) cptr l
    mapM_ (\i -> poke (castPtr $ p `plusPtr` i) (0 :: Word8)) [l..sizeOf c' - 1]

  peek p = do
    a <- peekArray (sizeOf (undefined :: CUTF8Char)) $ castPtr p
    return $ CUTF8Char $ T.head $ T.decodeUtf8 $ B.pack a

type Table = Ptr (Ptr CUTF8Char)

foreign import capi "crc-brute.h makeCrcTable" c_makeCrcTable :: IO ()
foreign import capi "crc-brute.h crcBruteUtf8" c_crcBruteUtf8 :: Table -> Ptr CInt -> CInt -> Ptr CInt
                                                                -> Word32
                                                                -> FunPtr Periodic -> FunPtr Check
                                                                -> Ptr CUTF8Char
                                                                -> IO CInt

foreign import ccall "wrapper" mkPeriodic :: Periodic -> IO (FunPtr Periodic)
foreign import ccall "wrapper" mkCheck :: Check -> IO (FunPtr Check)

withPtr :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withPtr = withForeignPtr . fst . VS.unsafeToForeignPtr0 . VS.fromList

mkTable :: [[Char]] -> ([Ptr CUTF8Char] -> IO a) -> IO a
mkTable table func = with [] table
  where -- there's surely a name for a monadic recursion like this
        with vecs [] = func $ reverse vecs
        with vecs (vec:t) = withPtr (map CUTF8Char vec) $ \p -> with (p:vecs) t

crcBruteUtf8 :: Bool -> [(Int, [Char])] -> Word32 -> (T.Text -> Bool) -> IO (Maybe T.Text)
crcBruteUtf8 verbose table target check = mkTable (map snd table) $ \tablePtrs -> do
  c_makeCrcTable
  threads <- getNumCapabilities
  withTaskGroup threads $ \tg -> do
    res <- newEmptyTMVarIO
    stop <- newTVarIO False
    num <- newTVarIO (0 :: Int)
  
    let plen = sum $ map fst table
      
        widths = map (fromIntegral . findWidths . snd) table
        findWidths (h:t) = if all ((== w) . width) t
                         then w
                         else error "findWidth: dynamic width is not supported"
          where w = width h
                width c = B.length $ T.encodeUtf8 $ T.singleton c
        findWidths _ = error "findWidth: empty list"

        stopWorker ws = do
          atomically $ writeTVar stop True
          mapM_ wait ws

        workRanges = perms $ zip (map fst table) (map pure [0..])
        workLen = length workRanges

        worker perm = allocaArray plen $ \str -> do
          workn <- atomically $ do
            modifyTVar num (+1)
            readTVar num
          let prefix = "worker " ++ show workn ++ "/" ++ show workLen ++ ": "
          when verbose $ putStrLn $ prefix ++ "starting"
          time <- getCPUTime >>= newIORef

          let periodic n = do
                stop' <- atomically $ readTVar stop
                fromBool <$> if stop' then return False else do
                  when verbose $ do
                    new <- getCPUTime
                    old <- readIORef time
                    let int = fromIntegral (new - old) / 10^(12-3::Int) :: Double
                    when verbose $ putStrLn $ prefix ++ "done " ++ show n ++ " hashes in " ++ show int ++ " ms"
                    writeIORef time new
                  return True

              checkWrap = do
                when verbose $ putStrLn $ prefix ++ "found possible solution"
                ws <- T.pack <$> map unCUTF8Char <$> peekArray plen str
                let ret = check ws
                when ret $ do
                  when verbose $ putStrLn $ prefix ++ "found solution!"
                  atomically $ putTMVar res ws
                return $ fromBool ret

              table' = map (tablePtrs !!) perm
              widths' = map (widths !!) perm
              ranges' = map (fromIntegral . length . snd . (table !!)) perm

          void $
            withPtr table' $ \table'' ->
            withPtr widths' $ \widths'' ->
            withPtr ranges' $ \ranges'' ->
            bracket (mkPeriodic periodic) freeHaskellFunPtr $ \periodic'' ->
            bracket (mkCheck checkWrap) freeHaskellFunPtr $ \check'' ->
            c_crcBruteUtf8 table'' widths'' (fromIntegral plen) ranges'' target periodic'' check'' str
          when verbose $ putStrLn $ prefix ++ "finished"

    bracket (mapM (async tg . worker) workRanges) stopWorker $ \ws -> do
      atomically $ (Just <$> takeTMVar res) `orElse` (Nothing <$ mapM_ waitSTM ws)
