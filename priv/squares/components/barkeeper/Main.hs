{-# LANGUAGE DeriveDataTypeable #-}

module Main
       where

import Data.Typeable
import Data.Data
import Text.JSON (decode, Result(Ok, Error))
import Text.JSON.Generic (fromJSON)
import System.IO (hFlush, stdout)

data InMsg = InMsg
             { problem :: String,
               state   :: String
             } deriving (Eq, Typeable, Data)

main :: IO()
main = readLoop

readLoop :: IO()
readLoop = do
  line <- getLine

  let propMsg = readJSON line :: Result InMsg
  case propMsg of
    Ok (InMsg prob st) ->
      putStrLn $ makeResponse [show (read st :: [Int]), prob]
    Error errorString ->
      putStrLn errorString

  hFlush stdout
  readLoop

readJSON :: Data a => String -> Result a
readJSON s = do
  res <- decode s
  fromJSON res

makeResponse :: [String] -> String
makeResponse ls = "{\"worker input\":" ++ show ls ++ "}"
