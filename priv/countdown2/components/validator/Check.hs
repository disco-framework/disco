{-# LANGUAGE CPP #-}

module Check (checkExpr)
where


import Data.Maybe (isJust, fromJust)
import Data.List  (sort, (\\))
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Expr as PE

import CountDown  (Op(..), Expr(..), eval, cost, vals)


type Res a = Either String a


checkExpr :: [Integer] -> Integer -> String -> (Integer, String)  -- (score, caption)
checkExpr numberlist result proposition = res $ check proposition
  where
    res (Left s)  = (-1, s)
    res (Right e) = (cost e, proposition)
    check         = (syntaxCheck
                 >>> wellformedCheck
                 >>> expectedResult result
                 >>> usedNumbers numberlist)
      where
        (>>>) :: (a -> Res b) -> (b -> Res c) -> (a -> Res c)
        (f >>> g) x = cont (f x)
          where
            cont (Left e)  = Left e
            cont (Right r) = g r

usedNumbers :: [Integer] -> Expr -> Res Expr
usedNumbers expected e | null diff = Right e
                       | otherwise = Left ("Unerlaubte Zahl(en) verwendet: " ++ (init . tail . show) diff)
  where
    used = vals e
    diff = sort used \\ sort expected

expectedResult :: Integer -> Expr -> Res Expr
expectedResult expected e | res /= expected = Left ("Resultat ist " ++ show res ++ ", erwartet wurde " ++ show expected)
                          | otherwise       = Right e
  where
    res = (fromJust . eval) e

wellformedCheck :: Expr -> Res Expr
wellformedCheck e | (isJust . eval) e = Right e
                  | otherwise         = Left ("Ungueltiger arithmetischer Ausdruck: " ++ show e)

syntaxCheck :: String -> Res Expr
syntaxCheck inp = either (Left . errorMsg) (Right . id)
                . P.parse (do
                             x <- exprParser
                             P.skipMany P.space
                             P.eof
                             return x
                          ) ""
                $ inp
  where
    errorMsg e = "Syntaxfehler bei: " ++ cutR 20 (cutL (pos - 1) inp)
      where
        pos                         = P.sourceColumn $ P.errorPos e
        cutL p s                    = (replicate (min p 3) '.') ++ (drop p s)
        cutR n s | length s > n + 3 = (take n s) ++ "..."
                 | otherwise        = s

exprParser :: P.Parser Expr
exprParser = PE.buildExpressionParser table factor P.<?> "Falscher Ausdruck"
  where
    skipSpace     = P.skipMany P.space
    table         = [ [ genOp "+"   (App Add)
                      , genOp "-"   (App Sub)
                      , genOp "*"   (App Mul)
                      , genOp "/"   (App Div)
#ifdef modulo
                      , genOp "%"   (App Mod)
#endif
                      , genOp "<+>" (App BPl)
                      , genOp "<->" (App BMi)
                      , genOp "<*>" (App BMu)
                      ]
                    ]
    genOp s f     = PE.Infix (genParser s f) PE.AssocNone
    genParser s f = P.try (skipSpace >> P.string s) >> return f
    factor        = P.between
                      (P.try (skipSpace >> P.char '('))
                      (P.try (skipSpace >> P.char ')'))
                      exprParser
                    P.<|> (do
                             skipSpace
                             n <- P.many1 P.digit
                             return $ Val $ read n
                          )
