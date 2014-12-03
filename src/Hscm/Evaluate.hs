module Hscm.Evaluate (eval) where


import Hscm.Parser

eval :: Expression -> Expression
eval expr@(Symbol _) = expr
eval expr@(Number _) = expr
eval expr@(Bool _) = expr
eval expr@(Character _) = expr
eval expr@(String _) = expr
eval (Vector _) = undefined
eval (List [Symbol "quote", val]) = val
eval (List (Symbol func : args)) = apply func $ map eval args

apply :: String -> [Expression] -> Expression
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [Expression] -> Expression)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("reminder", numericBinOp rem)]
              
numericBinOp :: (Integer -> Integer -> Integer) -> [Expression] -> Expression
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Expression -> Integer
unpackNum (Number n) = n
