module EvalApl where

import qualified ParseApl as P

data AplValue = Scalar Integer
              | Vector [Integer]
              | Matrix [Integer] [Integer] -- Dimension x Data
                deriving (Show)

eval :: P.Expr -> AplValue
eval (P.DyaApp se P.Add exp) =
    case (evalSubExpr se, eval exp) of
        (Scalar i, Scalar j) -> Scalar (i + j)
        (Scalar i, Vector v) -> Vector (map ((+) i) v)
        (Vector v, Scalar j) -> Vector (map ((+) j) v)
        (Vector v1, Vector v2) -> Vector (zipWith (+) v1 v2)

eval (P.SubE se) = evalSubExpr se

evalSubExpr :: P.SubExpr -> AplValue
evalSubExpr (P.SimE (P.AId (P.Const (P.Num [i])))) = Scalar i
evalSubExpr (P.SimE (P.AId (P.Const (P.Num v)))) = Vector v
evalSubExpr (P.SimE (P.EExp e)) = eval e
