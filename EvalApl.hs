module EvalApl where

import ParseApl as P
import Data.List
import qualified Data.List.Split as LS

data AplValue = Scalar Integer
              | Vector [Integer]
              | Matrix [Integer] [Integer] -- Dimension x Data


showMatrix :: [Integer] -> [Integer] -> String
showMatrix [d] v = show (Vector v)
showMatrix (d:ds) v =
    let subLists = LS.chunksOf (fromIntegral d) v
    in intercalate "\n" $ map (showMatrix ds) subLists

instance Show AplValue where
    show (Scalar i) = show i
    show (Vector v) = intercalate " " $ map show v
    show (Matrix dim v) = showMatrix (reverse dim) v

applyDyaFun :: (Integer -> Integer -> Integer) -> AplValue -> AplValue -> AplValue
applyDyaFun f (Scalar i) (Scalar j) = Scalar (f i j)
applyDyaFun f (Vector v) (Scalar j) = Vector (map (f j) v)
applyDyaFun f (Scalar i) (Vector v) = Vector (map (f i) v)
applyDyaFun f (Vector v) (Vector u) = Vector (zipWith f v u)

applyMonFun :: (Integer -> Integer) -> AplValue -> AplValue
applyMonFun f (Scalar i) = Scalar $ f i
applyMonFun f (Vector v) = Vector $ map f v
applyMonFun f (Matrix dim v) = Matrix dim $ map f v

getDyaFun :: P.DyaId -> (Integer -> Integer -> Integer)
getDyaFun P.Add = (+)
getDyaFun P.Subtract = (-)
getDyaFun P.Multiply = (*)
getDyaFun P.Divide = quot


eval :: P.Expr -> AplValue

eval (P.MonApp P.Not exp) =
    let not b = if b == 0 then 1 else 0
    in applyMonFun not (eval exp)

eval (P.MonApp P.Abs exp) = applyMonFun abs (eval exp)
eval (P.MonApp P.Neg exp) = applyMonFun ((-)0) (eval exp)

eval (P.MonApp P.Iota exp) =
    case eval exp of
        Scalar i -> Vector $ take (fromIntegral i) (iterate ((+) 1) (toInteger 1))

eval (P.MonApp P.Dim exp) =
    case eval exp of
        Scalar i -> Scalar 0
        Vector v -> Scalar $ toInteger (length v)
        Matrix dim _ -> Vector dim

eval (P.DyaApp se P.Add exp) = applyDyaFun (+) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Subtract exp) = applyDyaFun (-) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Divide exp) = applyDyaFun div (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Multiply exp) = applyDyaFun (*) (evalSubExpr se) (eval exp)
eval (P.DyaApp se P.Exp exp) = applyDyaFun (^) (evalSubExpr se) (eval exp)

eval (P.DyaApp se P.Reshape exp) =
    let sz = evalSubExpr se
        e  = eval exp
    in case (sz, e) of
        (Scalar i, Scalar j) -> Vector (replicate (fromIntegral i) j)
        (Scalar i, Vector v) -> Vector (take (fromIntegral i) (cycle v))
        (Scalar i, Matrix _ v) -> Vector (take (fromIntegral i) (cycle v))
        (Vector v, Scalar j) -> Matrix v (replicate (fromIntegral (foldr1 (*) v)) j)
        (Vector v, Vector u) -> Matrix v (take (fromIntegral (foldr1 (*) v)) (cycle u))
        (Vector v, Matrix _ u) -> Matrix v (take (fromIntegral (foldr1 (*) v)) (cycle u))


eval (P.MonOpApp dya P.Reduce expr) =
    case eval expr of
        Vector v -> Scalar (foldr1 (getDyaFun dya) v)
        Matrix [d] v -> Scalar (foldr1 (getDyaFun dya) v)
        Matrix [d0,d1] v -> Vector $ map (foldr1 (getDyaFun dya))  (LS.chunksOf (fromIntegral d1) v)


eval (P.MonOpApp dya P.Scan expr) =
    case eval expr of
        Vector v -> Vector (scanl1 (getDyaFun dya) v)

eval (P.SubE se) = evalSubExpr se

evalSubExpr :: P.SubExpr -> AplValue
evalSubExpr (P.SimE (P.AId (P.Const (P.Num [i])))) = Scalar i
evalSubExpr (P.SimE (P.AId (P.Const (P.Num v)))) = Vector v
evalSubExpr (P.SimE (P.EExp e)) = eval e
