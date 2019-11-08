module Eval (compute) where

import Exp
-- Purpose: to actually compute a Exp

-- Given list of true things and expr, evaluates
compute :: Exp -> [Int] -> Bool
compute (VAR x) xs = 
    x `elem` xs
compute (AND f g) xs =
    (compute f xs) && (compute g xs)
compute (OR f g) xs = 
    (compute f xs) || (compute g xs)
compute (NOT f) xs =
    not $ compute f xs
compute (XOR f g) xs = 
    (compute f xs /= compute g xs)
compute (BIIM f g) xs =
    (compute f xs == compute g xs)
compute (IMPL f g) xs =
    not (compute f xs) || compute g xs
        