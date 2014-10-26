module Main
       where

import Data.List (sortBy, nub)
--import Debug.Trace (trace)
import System.IO (hFlush, stdout)

import Types


-- TODO add disruptors
-- disruptors are bottom-left or top-right corners of free space.
-- using those, we can lay squares into holes at the edges of the field.
-- just keep the candidates where the distance to the field edge is smaller
-- than the distance in the other direction and add corresponding disruptors.
-- then when laying a square we have to check for disruptors in that area.
-- disruptors must not touch new squares anywhere but at the corners.

type Solution = [(Int, Int, Int)]
type Solver = [Int] -> Int -> [Solution]
type Pos = (Int, Int)

data Strategy = Fast | Complete

main :: IO()
main = do
  msg1 <- getLine
  msg2 <- getLine
  let
    squares = read msg1 :: [Int]
    squareToFill = read msg2 :: Int
    sols1 = filterNotBetter $ calcTree Fast (magicReduce squares squareToFill) squareToFill
    sols2 = filterNotBetter $ calcTree Complete (magicReduce squares squareToFill) squareToFill
    sols3 = filterNotBetter $ calcTree Complete squares squareToFill
  mapM_ (printSolution squareToFill) $ filterNotBetter $ concat [sols1, sols2, sols3]

printSolution :: Int -> Solution -> IO ()
printSolution _ sol = do
  print sol
  hFlush stdout

printSolutionD :: Int -> Solution -> IO ()
printSolutionD stf sol = do
  print sol
  print $ fromIntegral (filledArea sol) / fromIntegral (filledArea [(0,0,stf)])
  hFlush stdout

filterNotBetter :: [Solution] -> [Solution]
filterNotBetter = filterNotBetter' 0

filterNotBetter' :: Int -> [Solution] -> [Solution]
filterNotBetter' _ [] = []
filterNotBetter' acc (sol:sols)
  | sc > acc  = sol : filterNotBetter' sc sols
  | otherwise = filterNotBetter' acc sols
  where sc = score sol

rsort :: Ord a => [a] -> [a]
rsort = sortBy (flip compare)

calcTree :: Strategy -> Solver
calcTree strat squares squareToFill =
  let
    tonub = length squares > (length $ nub squares)
  in
  calcTree' strat tonub (rsort squares) squareToFill [(0,0)] []

calcTree' :: Strategy -> Bool -> [Int] -> Int -> [Pos] -> Solution -> [Solution]
calcTree' _ _ [] _ _ solution                          = [solution]
calcTree' strat tonub squares squareToFill candidates solution =
  solution :
  concatMap
  (\sq -> concatMap (\ (sol, cands) -> calcTree' strat tonub (squares' sq sol) squareToFill cands sol) $
          placeOneConstrained solution squareToFill sq candidates)
  (if tonub then nub squares else squares)
  where
    leftArea sol = squareToFill^(2::Int) - filledArea sol
    squares' sq sol = case strat of
      Complete -> dropWhile (\s -> s^(2::Int) > leftArea sol || s > (squareToFill - sq)) $ removeOne sq squares
      Fast     -> dropWhile (\s -> s^(2::Int) > leftArea sol || s > (squareToFill - sq)) $ tail $ dropWhile (/= sq) squares

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne v (e:es)
  | e == v    = es
  | otherwise = e : removeOne v es

magicReduce :: [Int] -> Int -> [Int]
magicReduce sqs stf =
  let
    sorted = rsort sqs
    cutval = ceiling $ fromIntegral stf * ((2/3) :: Double)
  in
   head sorted : dropWhile (> cutval) (tail sorted)

placeOneConstrained :: Placement -> Int -> Int -> [Pos] -> [(Solution, [Pos])]
placeOneConstrained sol squareToFill square candidates =
  let
    respectingLimits =
      filter (\ (x,y) -> (x+square) <= squareToFill && (y+square) <= squareToFill) candidates
  in
   map
   (\ (x,y) -> ((x,y,square):sol, newCandidates candidates (x,y,square) squareToFill)) $
   safeHead $ sortBy compareCandidates2 respectingLimits


-- calculate additional candidates and get rid of those that are now
-- hard to process
newCandidates :: [Pos] -> SquareSpec -> Int -> [Pos]
newCandidates oldCandidates (x,y,size) squareToFill =
  let
    filterIn n = filter (\ (x',y') -> x' < n && y' < n)
    new = filterIn squareToFill [(x+size,y),(x,y+size)]--,(x+size,y+size)]
  in
   new ++ filter (\ (x',y') -> x' >= x+size || y' >= y+size) oldCandidates

safeHead :: [a] -> [a]
safeHead [] = []
safeHead (e:_) = [e]

compareCandidates :: Pos -> Pos -> Ordering
compareCandidates (x1,y1) (x2,y2)
  | y1 == y2  = compare x1 x2
  | otherwise = compare y1 y2

compareCandidates2 :: Pos -> Pos -> Ordering
compareCandidates2 (x1,y1) (x2,y2) =
  compare (x1+y1) (x2+y2)

score :: Placement -> Int
score = filledArea

filledArea :: Placement -> Int
filledArea = foldr (\ (_, _, size) acc -> acc + size^(2::Int)) 0
