{-# LANGUAGE CPP #-}

module CountDown ( Op(..),
                   Expr(..),
                   eval,
                   cost,
                   vals )
where

data Op   = Add | Sub | Mul | Div | BPl | BMi | BMu
#ifdef modulo
            | Mod
#endif
            deriving (Eq, Ord)

data Expr = Val Integer | App Op Expr Expr
            deriving (Eq, Ord)

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
#ifdef modulo
  show Mod = " % "
#endif
  show BPl = " <+> "
  show BMi = " <-> "
  show BMu = " <*> "

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = bracket l ++ show o ++ bracket r
                   where
                     bracket (Val n) = show n
                     bracket e       = "(" ++ show e ++ ")"


valid :: Op -> Integer -> Integer -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
#ifdef modulo
valid Mod x y = x `mod` y /= 0
#endif
valid BPl _ _ = True
valid BMi x y = x > y
valid BMu x y = x > y

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
#ifdef modulo
apply Mod x y = x `mod` y
#endif
apply BPl x y = (x + y) * (x + y)
apply BMi x y = (x - y) * (x - y)
apply BMu x y = (x + y) * (x - y)

evalWithCost :: Expr -> (Integer, Integer)
evalWithCost (Val n)     = (n,n)
evalWithCost (App o l r) = (res, c1 + c2 + res)
                         where
                           (v1, c1) = evalWithCost l
                           (v2, c2) = evalWithCost r
                           res      = apply o v1 v2

eval :: Expr -> Maybe Integer
eval (Val n)     = return n
eval (App o l r) = do
                     v1 <- eval l
                     v2 <- eval r
                     if valid o v1 v2
                       then return (apply o v1 v2)
                       else (fail "")

cost :: Expr -> Integer
cost = snd . evalWithCost

vals :: Expr -> [Integer]
vals (Val v)     = [v]
vals (App _ l r) = vals l ++ vals r
