module Jumble (Jumble, readJumble, intToJumble, jumbleToInt,
  jAdd, jSub, jMul, jDiv, jLE, jLT, jGE, jGT, jBox, jExtend,
  post) where

import Shaped
import Data.Char
import Data.Ord
import Data.Ratio
import Text.Printf
import qualified Data.Vector as V

-- TODO: Infinity.
data Jumble = I Integer | X Integer | Q (Ratio Integer) | D Double | Z (Double, Double) | Box (Shaped Jumble) deriving Eq

jShowInt n
  | n < 0     = '_':jShowInt (-n)
  | otherwise = show n

jShowDouble n
  | n < 0        = '_':jShowDouble (-n)
  | '.' `elem` s = reverse $ dropWhile (== '0') $ reverse s
  | otherwise    = s
  where s = printf "%.6g" n

instance Ord Jumble where
  compare (I x) (I y) = compare x y
  compare (X x) (X y) = compare x y
  compare (Q x) (Q y) = compare x y
  compare (D x) (D y) = compare x y
  compare (Z x) (Z y) = error "domain error"
  compare x y = uncurry compare $ pro x y

instance Show Jumble where
  show (I x) = jShowInt x
  show (X x) = jShowInt x
  show (Q x) = jShowInt (numerator x) ++ ('r':show (denominator x))
  show (D x) = jShowDouble x
  show (Z (x, y)) = jShowDouble x ++ (if y == 0 then "" else 'j':jShowDouble y)
  show (Box x) = "[" ++ show x ++ "]"

readJumble :: String -> Maybe Jumble
readJumble s
  | all isDigit s = Just $ checkOverflow (read s :: Integer)
  | otherwise     = Nothing

intToJumble :: Integral a => a -> Jumble
intToJumble = checkOverflow . fromIntegral

jumbleToInt :: Jumble -> Int
jumbleToInt (I x) = fromIntegral x
jumbleToInt _ = error "domain error"

pro (I x) (I y) = (I x, I y)
pro (X x) (X y) = (X x, X y)
pro (Q x) (Q y) = (Q x, Q y)
pro (D x) (D y) = (D x, D y)
pro (Z x) (Z y) = (Z x, Z y)

pro (I x) (X y) = (X x, X y)
pro (I x) (Q y) = (Q $ x % 1, Q y)
pro (I x) (D y) = (D $ fromIntegral x, D y)
pro (I x) (Z y) = (Z (fromIntegral x, 0), Z y)

pro (X x) (Q y) = (Q $ x % 1, Q y)
pro (X x) (D y) = (D $ fromIntegral x, D y)
pro (X x) (Z y) = (Z (fromIntegral x, 0), Z y)

pro (Q x) (D y) = (D $ fromRational x, D y)
pro (Q x) (Z y) = (Z (fromRational x, 0), Z y)

pro (D x) (Z y) = (Z (x, 0), Z y)

pro (Box x) (Box y) = (Box x, Box y)

pro x y = pro y x

jBox = Box

maxint = 2^63 - 1
minint = -2^63

checkOverflow z
  | z >= minint && z <= maxint = I z
  | otherwise                  = D $ fromIntegral z

jMul (I x) (I y) = checkOverflow $ x * y
jMul (X x) (X y) = X (x * y)
jMul (Q x) (Q y) = Q (x * y)
jMul (D x) (D y) = D (x * y)
jMul (Z (a, b)) (Z (c, d)) = Z (a*c - b*d, a*d + b*c)
jMul x y = uncurry jMul $ pro x y

-- TODO: Check for division by zero.
jDiv (I x) (I y) = jDiv (D $ fromIntegral x) (D $ fromIntegral y)
jDiv (X x) (X y) = Q (x % y)
jDiv (Q x) (Q y) = Q (x / y)
jDiv (D x) (D y) = D (x / y)
jDiv (Z (a, b)) (Z (c, d)) = Z (a*c + b*d, b*c - a*d)
jDiv x y = uncurry jDiv $ pro x y

jAdd (I x) (I y) = checkOverflow $ x + y
jAdd (X x) (X y) = X (x + y)
jAdd (Q x) (Q y) = Q (x + y)
jAdd (D x) (D y) = D (x + y)
jAdd (Z (a, b)) (Z (c, d)) = Z (a + c, b + d)
jAdd x y = uncurry jAdd $ pro x y

jSub (I x) (I y) = checkOverflow $ x - y
jSub (X x) (X y) = X (x - y)
jSub (Q x) (Q y) = Q (x - y)
jSub (D x) (D y) = D (x - y)
jSub (Z (a, b)) (Z (c, d)) = Z (a - c, b - d)
jSub x y = uncurry jSub $ pro x y

jBool = I . fromIntegral . fromEnum

jLT x y = jBool $ x < y
jLE x y = jBool $ x <= y
jGT x y = jBool $ x > y
jGE x y = jBool $ x >= y

jExtend (I x) = X x
jExtend (X x) = X x
jExtend (Q x) = Q x
jExtend (D x) = Q $ approxRational x 0.0000001
jExtend (Z x) = error "domain error"

post :: [Int] -> [Shaped Jumble] -> Shaped Jumble
post frame xs = typeMatch $ homogenize (intToJumble 0) frame xs

typeMatch :: Shaped Jumble -> Shaped Jumble
typeMatch arg@(Shaped rs xs)
  | V.null xs = arg
  | otherwise = let
    b :: Jumble
    b = V.foldl1' ((fst .) . pro) xs
    in Shaped rs $ fst . (`pro` b) <$> xs
