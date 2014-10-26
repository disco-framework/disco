{-# LANGUAGE DeriveDataTypeable #-}

module Main
       where

import Data.Typeable
import Data.Data
import Data.Maybe (fromJust)
import Data.Aeson (encode, decode)
import Data.Aeson.Generic (toJSON, fromJSON)
import Data.Aeson.Types (Result(Success,Error))
import System.IO (hFlush, stdout)
import Data.ByteString.Lazy.Char8 as BSL (ByteString, lines, putStrLn, getContents, pack)
import Data.ByteString.Char8 as BS (ByteString, readInt, length, tail)

import Check (checkProp)

data PropositionMsg = PropositionMsg
                      { input :: [BS.ByteString],
                        output :: BS.ByteString
                      } deriving (Eq, Typeable, Data)

data ScoreMsg = ScoreMsg { score :: Int, caption :: String }
              deriving (Eq, Show, Typeable, Data)

main :: IO()
main = Main.interact makeIO

interact :: (BSL.ByteString -> [BSL.ByteString]) -> IO ()
interact transformer = mapM_ putLine . transformer =<< BSL.getContents

putLine :: BSL.ByteString -> IO ()
putLine line = do
  BSL.putStrLn line
  hFlush stdout

makeIO :: BSL.ByteString -> [BSL.ByteString]
makeIO inp =
  map processRequest $ BSL.lines inp

processRequest :: BSL.ByteString -> BSL.ByteString
processRequest line =
  let
    propMsg = readJSON line :: Result PropositionMsg
  in
  case propMsg of
    Success msg ->
      makeJSON $ validate msg
    Error _ ->
      pack $ "JSON error: " ++ show line

makeJSON :: Data a => a -> BSL.ByteString
makeJSON = encode . toJSON

readJSON :: Data a => BSL.ByteString -> Result a
readJSON s = do
  res <- maybe (Error "") Success $ decode s
  fromJSON res

validate :: PropositionMsg -> ScoreMsg
validate (PropositionMsg [nums, res] prop) =
  ScoreMsg scoreVal captionString
  where
    (scoreVal, captionString) = checkProp (readIntlistFromByteString nums) (readIntFromByteString res) prop
validate _ = error "Wrong PropositionMsg format"

readIntFromByteString :: BS.ByteString -> Int
readIntFromByteString = fst . fromJust . BS.readInt

readIntlistFromByteString :: BS.ByteString -> [Int]
readIntlistFromByteString bs
  | 2 >= BS.length bs = []
  | otherwise         =
    let
      (n, rest) = fromJust $ BS.readInt $ BS.tail bs
    in
     n : readIntlistFromByteString rest
