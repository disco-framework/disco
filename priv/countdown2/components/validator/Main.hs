{-# LANGUAGE DeriveDataTypeable #-}

module Main
where


import Data.Data         (Data, Typeable)
import System.IO         (hFlush, stdout)
import Text.JSON         (decode, encode, Result(Ok, Error))
import Text.JSON.Generic (fromJSON, toJSON)

import Check             (checkExpr)


data PropositionMsg = PropositionMsg { input :: [String],
                                       output :: String }
                      deriving (Eq, Show, Typeable, Data)

data ScoreMsg       = ScoreMsg { score :: Integer,
                                 caption :: String }
                      deriving (Eq, Show, Typeable, Data)


main :: IO()
main = readLoop

readLoop :: IO()
readLoop = do
             line <- getLine
             let propMsg = readJSON line :: Result PropositionMsg
             case propMsg of
               Ok msg            -> putStrLn $ makeJSON $ validate msg
               Error errorString -> putStrLn errorString
             hFlush stdout
             readLoop

makeJSON :: Data a => a -> String
makeJSON = encode . toJSON

readJSON :: Data a => String -> Result a
readJSON s = do
               res <- decode s
               fromJSON res

validate :: PropositionMsg -> ScoreMsg
validate (PropositionMsg [nums, res] prop) = ScoreMsg scoreVal captionString
  where
    (scoreVal, captionString) = checkExpr (read nums) (read res) prop
validate _ = error "Wrong PropositionMsg format"
