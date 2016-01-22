module Shaped (Shaped(..), fromList, shapeList, singleton, go1, go2, homogenize) where

import Data.List
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

data Shaped a = Shaped [Int] (Vector a) deriving Eq

showK xs shapeX k =
  case shapeX of
    []     -> show $ xs!k
    [n]    -> unwords $ showK xs [] <$> (k +) <$> [0..n-1]
    (n:ns) -> unlines $ showK xs ns <$> (k +) <$> (product ns *) <$> [0..n-1]
    
instance Show a => Show (Shaped a) where
  show (Shaped shapeX xs) = showK xs shapeX 0

singleton :: a -> Shaped a
singleton x = Shaped [] (V.singleton x)

fromList :: [a] -> Shaped a
fromList xs = Shaped [length xs] (V.fromList xs)

shapeList :: [Int] -> [a] -> Shaped a
shapeList shape xs =
  Shaped shape (V.fromList $ take (product shape) (cycle xs))

fill :: a -> [Int] -> Shaped a -> Vector a
fill z newRank (Shaped rank vs)
  | newRank == rank = vs
  | otherwise       = V.replicate (product newRank) z V.// 
    zip (sum . zipWith (*) (scanl1 (*) (1:reverse newRank)) . reverse <$>
    sequence (flip take [0..] <$> rank)) (V.toList vs)

homogenize :: a -> [Int] -> [Shaped a] -> Shaped a
homogenize z frame xs = let
  origs = map (\(Shaped rs _) -> rs) xs
  m = maximum $ length <$> origs
  exts = map (\xs -> replicate (m - length xs) 1 ++ xs) origs
  resultRank = foldl1' (zipWith max) exts
  in Shaped (frame ++ resultRank) $ V.concat $ fill z resultRank <$> xs

go1 :: a -> Int -> (Shaped a -> Shaped a) -> Shaped a -> Shaped a
go1 z r op (Shaped shape xs) = let
  (frame, rank) = splitAt (length shape - min r (length shape)) shape
  sz = product rank
  xn = V.length xs `div` sz
  in homogenize z frame [op (Shaped rank $ V.slice (i*sz) sz xs) | i <- [0..xn-1]]

go2 :: a -> Int -> Int -> (Shaped a -> Shaped a -> Shaped a) ->
  Shaped a -> Shaped a -> Shaped a
go2 z rL rR op (Shaped shapeX xs) (Shaped shapeY ys)
  | or $ zipWith (/=) frameX frameY = error "frame mismatch"
  | length frameX > length frameY =
    f (flip op) (frameY, rankY) ys (frameX, rankX) xs
  | otherwise =
    f        op (frameX, rankX) xs (frameY, rankY) ys
  where
    dimL = length shapeX - min rL (length shapeX)
    dimR = length shapeY - min rR (length shapeY)
    (frameX, rankX) = splitAt dimL shapeX
    (frameY, rankY) = splitAt dimR shapeY

    f op (frameX, rankX) xs (frameY, rankY) ys = let
      xsize = product rankX
      ysize = product rankY
      m = div (V.length ys * xsize) (V.length xs * ysize)
      xn = V.length xs `div` xsize
      in homogenize z frameY $ concat [[op (Shaped rankX $ V.slice (i*xsize) xsize xs) (Shaped rankY $ V.slice ((i*m + j)*ysize) ysize ys) | j <- [0..m-1] ] | i <- [0..xn-1]]

{-
--hDyad :: (Int -> Int -> Int) -> Noun -> Noun -> Noun
hDyad op x@(NounInt (Shaped shapeX _)) y@(NounInt (Shaped shapeY _))
  | or $ zipWith (/=) shapeX shapeY = undefined
  | length shapeX > length shapeY   = hDyad' (flip op) y x
  | otherwise                       = hDyad'       op  x y

hDyad' op (NounInt (Shaped shapeX xs)) (NounInt (Shaped shapeY ys)) =
  let dy = div (V.length ys) $ product shapeX
  in NounInt $ Shaped shapeY (V.zipWith op (V.concatMap (V.replicate dy) xs) ys)
-}
