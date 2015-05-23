import Control.Arrow
import Control.Monad
import Data.Word
import Data.Bits
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async.Pool
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16

update :: Int -> a -> [a] -> [a]
update i x = map (\(i', x') -> if i == i' then x else x') . zip [0..]

perms :: [Int] -> [[Int]]
perms lens = check $ filter ((/= 0) . snd) $ zip [0..length lens - 1] lens
  where check [(i, c)] = [replicate c i]
        check x = concatMap perm x

        perm (i, c) = map (i :) $ perms $ update i (c - 1) lens

type Callback = IO CInt

type Table = Ptr (Ptr Word16)

foreign import capi "crc-brute.h crcBruteUtf8_16" c_crcBruteUtf8_16 :: Ptr CInt -> CInt -> Table -> Ptr CInt -> Word32 -> Ptr CInt -> FunPtr Callback -> Ptr Word16 -> IO CInt

foreign import ccall "wrapper" mkCallback :: Callback -> IO (FunPtr Callback)

withPtr :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withPtr = withForeignPtr . fst . VS.unsafeToForeignPtr0 . VS.fromList

mkTable_16 :: [VU.Vector Word16] -> (Ptr CInt -> Table -> IO a) -> IO a
mkTable_16 table func = do
  let table' = map (fst . VS.unsafeToForeignPtr0 . VS.convert) table
  with [] table'
  where with vecs [] = withPtr (map (fromIntegral . VU.length) table) $ \lens ->
                       withPtr vecs $ \chars ->
                       func lens chars
        with vecs (vec:t) = withForeignPtr vec $ \p -> with (vecs ++ [p]) t

crcBruteUtf8_16 :: [(Int, VU.Vector Word16)] -> Word32 -> (B.ByteString -> Bool) -> IO (Maybe B.ByteString)
crcBruteUtf8_16 ranges target check = do
  threads <- getNumCapabilities
  mkTable_16 (map snd ranges) $ \lens chars ->
    alloca $ \stop ->
    withTaskGroup threads $ \tg -> do
      poke stop 0
      res <- newEmptyTMVarIO
      num <- newTVarIO (0 :: Int)
  
      let plen = sum $ map fst ranges
      
          stopWorker ws = do
            poke stop 1
            mapM_ cancel ws
      
          peekUtf8 plen str = B.pack <$> concatMap packUtf8 <$> peekArray plen str
      
          packUtf8 :: Word16 -> [Word8]
          packUtf8 w
            | w .&. 0xff00 > 0 = [low, high]
            | otherwise = [low]
            where low = fromIntegral $ w .&. 0xff
                  high = fromIntegral $ shiftR w 8

          workRanges = perms $ map fst ranges

          worker perm = withPtr (map fromIntegral perm) $ \perm' ->
                        allocaBytes (sizeOf (undefined :: Word16) * plen) $ \str ->
                        let checkWrap = do
                              ws <- peekUtf8 plen str
                              return $ fromBool $ check ws
                        in bracket (mkCallback checkWrap) freeHaskellFunPtr $ \cb -> do
                          n <- atomically $ do
                            modifyTVar num (+1)
                            readTVar num
                          putStrLn $ "starting worker " ++ show n ++ " of " ++ show (length workRanges)
                          r <- toBool <$> c_crcBruteUtf8_16 perm' (fromIntegral plen) chars lens target stop cb str
                          when r $ do
                            ws <- peekUtf8 plen str
                            atomically $ putTMVar res ws

      bracket (mapM (async tg . worker) workRanges) stopWorker $ \ws -> do
        atomically $ (Just <$> readTMVar res) `orElse` (Nothing <$ mapM_ waitSTM ws)

{-
  crc32 hash, sha1 hash
  13 russian letters, 3 ascii symbols
  utf8 encoding
-}

myRanges :: [(Int, VU.Vector Word16)]
myRanges = map (second $ VU.fromList . map (fromIntegral . fromEnum)) $
           [ (13, ['а'..'я'] ++ ['А'..'Я'])
           , (3, [' ', '!'])
           ]

myCRC32 :: Word32
myCRC32 = 0xfb610011

mySHA1 :: B.ByteString
mySHA1 = fst $ B16.decode "35bfe051be8b10feb2c92f4daed7d2f2e387bb68"

main :: IO ()
main = do
  res <- crcBruteUtf8_16 myRanges myCRC32 ((== mySHA1) . SHA1.hash)
  print res
