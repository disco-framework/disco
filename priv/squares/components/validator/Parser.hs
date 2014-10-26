module Parser
       (
         parsePlacement,
         test
       )
       where

import Types

import Data.Attoparsec.ByteString.Char8 (Parser, parse, decimal, skipSpace, char, IResult(..))
import Control.Applicative ((<*), (*>))
import Data.Attoparsec.Combinator (sepBy)
import Data.ByteString.Char8 (ByteString, pack, unpack, empty)

test :: String -> Either String Placement
test = parsePlacement . pack

parsePlacement :: ByteString -> Either String Placement
parsePlacement = handleParseResult . parse placement

handleParseResult :: IResult ByteString Placement -> Either String Placement
handleParseResult (Fail rest _contexts _msg) = Left $ "..." ++ shortenString 10 (unpack rest)
handleParseResult (Partial f)              = handleParseResult $ f empty
handleParseResult (Done _rest plac)        = Right plac

shortenString :: Int -> String -> String
shortenString n s =
  let
    (short, rest) = splitAt n s
  in
   if null rest then
     short
   else
     take (n-3) short ++ "..."

placement :: Parser Placement
placement = do
  skipSpace
  _ <- char '['
  skipSpace
  ((squareSpec <* skipSpace) `sepBy` (char ',' *> skipSpace)) <* char ']'

squareSpec :: Parser SquareSpec
squareSpec = do
  _ <- char '('
  skipSpace
  x <- decimal
  comma
  y <- decimal
  comma
  size <- decimal
  skipSpace
  _ <- char ')'
  return (x, y, size)

comma :: Parser ()
comma = do
  skipSpace
  _ <- char ','
  skipSpace
  return ()