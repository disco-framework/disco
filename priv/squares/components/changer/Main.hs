{-# LANGUAGE DeriveDataTypeable #-}

module Main
       where

import Data.Typeable
import Data.Data
import Data.List ((\\))
import Text.JSON (decode, Result(Ok, Error))
import Text.JSON.Generic (fromJSON)
import System.IO (hFlush, stdout)

type Placement = [(Int, Int, Int)]

data ChangeMsg = ChangeMsg
                 { proposition :: String,
                   state       :: String
                 } deriving (Eq, Typeable, Data)

main :: IO()
main = readLoop

readLoop :: IO()
readLoop = do
  line <- getLine

  let changeReq = readJSON line :: Result ChangeMsg
  case changeReq of
    Ok (ChangeMsg plc st) ->
      putStrLn $ makeResponse $ applyChange (read plc) (read st)
    Error errorString ->
      putStrLn errorString

  hFlush stdout
  readLoop

applyChange :: Placement -> [Int] -> [Int]
applyChange plc st = st \\ usedSquares
  where usedSquares = map (\ (_, _, size) -> size) plc

readJSON :: Data a => String -> Result a
readJSON s = do
  res <- decode s
  fromJSON res

makeResponse :: [Int] -> String
makeResponse st = "{\"state\":\"" ++ show st ++ "\"}"