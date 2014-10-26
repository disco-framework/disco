module Check
       (
         checkProp,
         simpleTest,
         score,
         hasOverlap
       )
       where

import Types
import Parser (parsePlacement)

import Data.List (sortBy, tails, sort)
import Data.ByteString.Char8 (ByteString, pack)

-- IDEA: parallelize checking?

type Res a = Either String a

checkProp :: [Int] -> Int -> ByteString -> (Int, String) -- squarelist, squareToFill, proposition
checkProp ns n prop =
  res (check prop)
  where
    res (Left s) =
      (-1, s)
    res (Right p) =
      (score p, successCaption p n)
    check =
      syntaxCheck
      >>> usedSquares ns
      >>> overlapCheck
      >>> containerLimitsCheck n

(>>>) :: (a -> Res b) -> (b -> Res c) -> (a -> Res c)
(f >>> g) x =
  cont (f x)
  where
    cont (Left e)  = Left e
    cont (Right r) = g r

successCaption :: Placement -> Int -> String
successCaption p n =
  show (length p) ++ " Quadrate, " ++
  show (roundTo 4 percentage)  ++ "% bedeckt"
  where
    percentage =
      (fromIntegral $ filledArea p :: Double) / fromIntegral n^(2::Int) * 100

syntaxCheck	:: ByteString -> Res Placement
syntaxCheck inp =
  either
  (\ errPos -> Left $ "Syntaxfehler: " ++ errPos)
  Right
  (parsePlacement inp)

score :: Placement -> Int
score = filledArea

filledArea :: Placement -> Int
filledArea = foldr (\ (_, _, size) acc -> acc + size^(2::Int)) 0

usedSquares :: [Int] -> Placement -> Res Placement
usedSquares given p =
  maybe (Right p) Left $
  usedSquares' (sort given) (sort $ map (\(_, _, size) -> size) p)

usedSquares' :: [Int] -> [Int] -> Maybe String
usedSquares' _ []        = Nothing
usedSquares' [] notGiven = Just $ "Nicht gegebene Quadrate benutzt: " ++ show notGiven
usedSquares' (g:gs) (u:us)
  | g == u    = usedSquares' gs us
  | g < u     = usedSquares' gs (u:us)
  | otherwise = -- g > u
    Just $ "Nicht gegebenes Quadrat benutzt: " ++ show u

containerLimitsCheck :: Int -> Placement -> Res Placement
containerLimitsCheck csize p =
  case filter (not . respectsLimits) p of
    [] -> Right p
    violating -> Left $ "Ausserhalb liegende Quadrate: " ++ show violating
  where
    respectsLimits (x, y, size) =
      valid x size && valid y size
    valid dim size =
      dim >= 0 && dim + size <= csize

-- TODO abort if overlap found
overlapCheck :: Placement -> Res Placement
overlapCheck p =
  case overlappingSquares of
    [] -> Right p
    l  -> Left $ "Überschneidungen: " ++ show l
  where sortedP = sortBy (\ (x1, _, _) (x2, _, _) -> compare x1 x2) p
        overlappingSquares =
          filter (\(_,overlaps) -> not $ null overlaps) $
          foldr foldFunc [] $ tails sortedP
        foldFunc (sq:rest) acc = analyzeOne sq rest : acc
        foldFunc [] acc = acc
        analyzeOne sq rest =
          (sq,
           filter (isOverlapping sq)
           (takeWhile (not . isClearRightOf sq)
            rest))

hasOverlap :: Placement -> Bool
hasOverlap p =
  either (const True) (const False) $ overlapCheck p

isClearRightOf :: SquareSpec -> SquareSpec -> Bool
isClearRightOf (x1, _, size) (x2, _, _) = x2 > (x1 + size)

isOverlapping :: SquareSpec -> SquareSpec -> Bool
isOverlapping (x1, y1, size1) (x2, y2, size2) =
  isHorizontalOverlapping && isVerticalOverlapping
  where
    isHorizontalOverlapping = (x1 < (x2 + size2)) && ((x1 + size1) > x2)
    isVerticalOverlapping   = (y1 < (y2 + size2)) && ((y1 + size1) > y2)

roundTo :: (Floating a, RealFrac a) => Int -> a -> a
roundTo digits val =
  (fromIntegral :: RealFrac a =>  Int -> a) (round $ val * 10^digits) / 10^digits

--
--
--

isInvalid :: (Int, String) -> Bool
isInvalid (0, _) = True
isInvalid _      = False

simpleTest :: Bool
simpleTest =
  and [
    -- overlapping
    isInvalid $ checkProp [1,3] 42 (pack "[(2,3,1), (1,1,3)]"),
    not $ isInvalid $ checkProp [1,2,3] 42 (pack "[(2,3,1), (1,1,2)]"),
    isInvalid $ checkProp [1,3] 42 (pack "[(1,2,1), (1,1,3)]"),
    not $ isInvalid $ checkProp [1,3] 42 (pack "[(1,4,1), (1,1,3)]"),
    -- squares usage
    isInvalid $ checkProp [1,2,3] 42 (pack "[(0,0,4)]"),
    -- container limits
    isInvalid $ checkProp [1,2,3] 42 (pack "[(-1,0,1)]"),
    isInvalid $ checkProp [1,2,3] 42 (pack "[(0,-1,1)]"),
    isInvalid $ checkProp [1,2,3] 42 (pack "[(0,42,1)]"),
    isInvalid $ checkProp [1,2,3] 42 (pack "[(42,0,1)]"),
    -- syntax
    not $ isInvalid $ checkProp [1] 42 (pack "[ ( 0 , 0 , 1 ) ]"),
    isInvalid $ checkProp [1] 42 (pack "["),
    isInvalid $ checkProp [1] 42 (pack "[(]"),
    isInvalid $ checkProp [1] 42 (pack "[(1,2)]")
    ]
