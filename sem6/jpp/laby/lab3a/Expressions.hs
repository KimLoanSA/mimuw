module Expressions where


data Exp = EInt Int             -- stała całkowita
         | EAdd Exp Exp         -- e1 + e2
         | ESub Exp Exp         -- e1 - e2
         | EMul Exp Exp         -- e1 * e2
         | EVar String          -- zmienna
         | ELet String Exp Exp  -- let var = e1 in e2
  deriving (Eq)


precedence :: Exp -> Int
precedence (EInt _)     = 3
precedence (EAdd _ _)   = 1
precedence (ESub _ _)   = 1
precedence (EMul _ _)   = 2
precedence (EVar _)     = 3
precedence (ELet _ _ _) = 4


-- 3 * (5 + 5)
-- 5 + (5 + 5) --> 5 + 5 + 5
instance Show Exp where
  show = showHelp 0
    where
      showHelp i elem = addParens i (precedence elem) $ showWithoutParent elem

      showWithoutParent (EInt i)             = show i
      showWithoutParent (EVar v)             = v
      showWithoutParent exp@(EAdd e1 e2)     = showHelp (precedence exp) e1 ++ " + " ++ showHelp (precedence exp) e2
      showWithoutParent exp@(ESub e1 e2)     = showHelp (precedence exp) e1 ++ " - " ++ showHelp (precedence exp) e2
      showWithoutParent exp@(EMul e1 e2)     = showHelp (precedence exp) e1 ++ " * " ++ showHelp (precedence exp) e2
      showWithoutParent exp@(ELet var e1 e2) = "let " ++ var ++ " = " ++ showHelp (precedence exp) e1 ++ " in " ++ showHelp (precedence exp) e2

      addParens p1 p2 exp
        | p1 <= p2  = exp
        | otherwise = "(" ++ exp ++ ")"


instance Num Exp where
    e1 + e2 = EAdd e1 e2
    e1 - e2 = ESub e1 e2
    e1 * e2 = EMul e1 e2
    signum e1 = undefined
    abs e1 = undefined
    fromInteger = EInt . fromInteger
    negate e1 = EMul (EInt (-1)) e1


simplify :: Exp -> Exp
simplify (EMul x (EInt 0)) = EInt 0
simplify (EMul (EInt 0) x) = EInt 0
simplify (EAdd x y)        = simplifyOnce $ simplify x + simplify y
simplify (ESub x y)        = simplifyOnce $ simplify x - simplify y
simplify (EMul x y)        = simplifyOnce $ simplify x * simplify y
simplify x                 = x

simplifyOnce :: Exp -> Exp
simplifyOnce (EAdd 0 x)               = x
simplifyOnce (EAdd x        0)        = x
simplifyOnce (EAdd (EInt x) (EInt y)) = EInt $ x + y

simplifyOnce (ESub x        0)        = x
simplifyOnce (ESub (EInt x) (EInt y)) = EInt $ x - y

simplifyOnce (EMul x        1)        = x
simplifyOnce (EMul 1        x)        = x
simplifyOnce (EMul (EInt x) (EInt y)) = EInt $ x * y
simplifyOnce (EMul x        (EInt y)) = EMul (EInt y) x

simplifyOnce exp@(EAdd (EMul (EInt n) x) (EMul (EInt m) y))
    | x == y    = EMul (EInt $ n + m) x
    | otherwise = exp

simplifyOnce x = x
