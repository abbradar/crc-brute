import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Digest.CRC32
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text.Encoding as T

import Utils
import CRCBrute

myRanges :: [(Int, [Char])]
myRanges = [ (3, ['а'..'в'])
           , (3, ['_', '!', ':'])
           ]

bruteTest :: Assertion
bruteTest = mapM_ one $ perms myRanges
  where one (T.pack -> str) = do
          let strB = T.encodeUtf8 str
              myCRC32 = crc32 strB
              mySHA1 = SHA1.hash strB
          res <- crcBruteUtf8 False myRanges myCRC32 ((== mySHA1) . SHA1.hash . T.encodeUtf8)
          case res of
           Nothing -> assertFailure $ "result not found for string: " ++ T.unpack str
           Just r -> assertEqual "result differs" r str

main :: IO ()
main = defaultMain $ testGroup "Test crc-brute"
       [ testCase "Find all words for given range" bruteTest
       ]
