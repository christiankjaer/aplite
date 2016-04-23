module EvalApl where

import ParseApl as P
import Data.List

data AplValue = Scalar Integer
              | Vector [Integer]
              | Matrix [Integer] [Integer] -- Dimension x Data

instance Show AplValue where
    show (Scalar i) = show i
    show (Vector v) = intercalate " " $ map show v

applyDyaFun :: (Integer -> Integer -> Integer) -> AplValue -> AplValue -> AplValue
applyDyaFun f (Scalar i) (Scalar j) = Scalar (f i j)
applyDyaFun f (Vector v) (Scalar j) = Vector (map (f j) v)
applyDyaFun f (Scalar i) (Vector v) = Vector (map (f i) v)
applyDyaFun f (Vector v) (Vector u) = Vector (zipWith f v u)

getDyaFun :: P.DyaId -> (Integer -> Integer -> Integer)
getDyaFun P.Add = (+)
getDyaFun P.Subtract = (-)
getDyaFun P.Multiply = (*)
getDyaFun P.Divide = quot


eval :: P.Expr -> AplValue

eval (P.MonApp P.Iota exp) =
    case eval exp of
        Scalar i -> Vector $ take (fromIntegral i) (iterate ((+) 1) (toInteger 1))

eval (P.DyaApp se P.Add exp) = applyDyaFun (+) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Subtract exp) = applyDyaFun (-) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Divide exp) = applyDyaFun div (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Multiply exp) = applyDyaFun (*) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Exp exp) = applyDyaFun (^) (evalSubExpr se) (eval exp)

eval (P.MonOpApp dya P.Reduce expr) =
    case eval expr of
        Vector v -> Scalar (foldr1 (getDyaFun dya) v)
eval (P.MonOpApp dya P.Scan expr) =
    case eval expr of
        Vector v -> Vector (scanl1 (getDyaFun dya) v)

eval (P.SubE se) = evalSubExpr se

evalSubExpr :: P.SubExpr -> AplValue
evalSubExpr (P.SimE (P.AId (P.Const (P.Num [i])))) = Scalar i
evalSubExpr (P.SimE (P.AId (P.Const (P.Num v)))) = Vector v
evalSubExpr (P.SimE (P.EExp e)) = eval e
