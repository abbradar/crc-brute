import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text.Encoding as T

import CRCBrute

{-
  crc32 hash, sha1 hash
  13 russian letters, 3 ascii symbols
  utf8 encoding
-}

myRanges :: [(Int, [Char])]
myRanges = [ (13, ['а'..'я'] ++ ['А'..'Я'])
           , (3, [' ', '!'])
           ]

myCRC32 :: Word32
myCRC32 = 0xfb610011

mySHA1 :: B.ByteString
mySHA1 = fst $ B16.decode "35bfe051be8b10feb2c92f4daed7d2f2e387bb68"

main :: IO ()
main = do
  res <- crcBruteUtf8 True myRanges myCRC32 ((== mySHA1) . SHA1.hash . T.encodeUtf8)
  print res
