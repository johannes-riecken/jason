{-# LANGUAGE DeriveGeneric, DerivingVia #-}
module Shaped (Shaped(..), fromList, shapeList, go1, go2, homogenize, keepChunking, slice, replicate') where

import Data.List
import qualified Data.List as V
import qualified Data.Vector as VV
import Safe.Exact
import Text.Printf
import GHC.Generics
import Generic.Data
import Safe
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Comonad
import Control.Monad

-- TODO: not total
(//) :: NonEmpty a -> NonEmpty (Int, a) -> NonEmpty a
(//) xs updates = NE.fromList . VV.toList $ (VV.//) (VV.fromList $ NE.toList xs) (NE.toList updates)

slice :: Int -> Int -> NonEmpty a -> NonEmpty a
slice i n = NE.fromList . take n . NE.drop i

data Shaped a = Shaped [Int] (NonEmpty a)
    deriving (Eq, Generic1)
    -- deriving (Show)
    deriving (Functor, Applicative) via Generically1 Shaped

instance Arbitrary a => Arbitrary (Shaped a) where
    arbitrary = Shaped <$> arbitrary <*> ((:|) <$> arbitrary <*> arbitrary)

instance Comonad Shaped where
    extract (Shaped _ xs) = NE.head xs
    duplicate (Shaped rs xs) = Shaped rs (extend (Shaped rs) xs)

-- format ensures column alignment
format :: [String] -> [String]
format xs = map unwords ann where
    ann = map (zipWith (printf "%*s") maxLengths) xs'
    maxLengths = map maximum $ transpose ys
    xs' = map words xs
    ys = map (map length) xs'

separator :: Int -> String
separator 0 = " "
separator n = replicate n '\n'

-- keepChunking creates a list containing at most a string with chunks the size
-- of the axes in order joined with that level's separator
keepChunking :: Show a => Shaped a -> NonEmpty String
keepChunking (Shaped axes ys) = snd $ foldl'
    (\(level, ys) x -> (succ level, intercalate (separator level) . (if x < 0 then reverse else id) <$> chunk (abs x) ys))
    (0, fmap show ys) axes

-- chunk splits a list into chunks of size n, discarding partial chunks at the
-- end
-- TODO: Doesn't terminate if n is 0
chunk :: Int -> NonEmpty a -> NonEmpty [a]
chunk = NE.unfoldr . splitAtExactMay'

splitAtExactMay' :: Int -> NonEmpty a -> ([a], Maybe (NonEmpty a))
splitAtExactMay' = undefined

instance Show a => Show (Shaped a) where
  show (Shaped shapeX xs) = intercalate "\n" . format . lines . NE.head $ keepChunking (Shaped (reverse shapeX) xs)

-- converts a Haskell list to a J 1-cell
fromList :: NonEmpty a -> Shaped a
fromList xs = Shaped [NE.length xs] xs

-- repeats a Haskell list to fill the given shape
-- TODO: fails with empty list, maybe replace with non-empty list
shapeList :: [Int] -> NonEmpty a -> Shaped a
shapeList shape xs =
  Shaped shape (NE.fromList $ NE.take (product (map abs shape)) (NE.cycle xs))

-- fill pads with zero values to match the new shape
-- example:
-- 1 2
-- 3 4
-- padded to [3,3] with zero-value 7 becomes
-- 1 2 7
-- 3 4 7
-- 7 7 7
-- TODO: not total
-- TODO: rewrite using "these" module
fill :: a -> [Int] -> Shaped a -> NonEmpty a
fill z newShape (Shaped shape vs)
  | newShape == shape = vs
  | otherwise       = replicate' (product newShape) z //
    NE.zip (NE.fromList $ sum . NE.zipWith (*) (NE.scanl (*) 1 (reverse newShape)) . NE.reverse <$>
    -- for shape [2,3] this would be [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]
    sequence (flip take [0..] <$> NE.fromList shape)) vs

homogenize :: a -> [Int] -> NonEmpty (Shaped a) -> Shaped a
homogenize z frame xs = let
  origShapes = fmap (\(Shaped rs _) -> NE.fromList rs) xs
  -- m is maximum rank
  m = maximum $ NE.length <$> origShapes
  -- exts pads ones left of each shapes to match the maximum rank
  exts = fmap (\xs -> replicate' (m - NE.length xs) 1 <> xs) origShapes
  -- resultShape has for each axis the maximum of all shapes
  resultShape = foldl1' (zipWith max) (NE.toList . fmap NE.toList $ exts)
  in Shaped (frame <> resultShape) $ join $ fill z resultShape <$> xs

-- Takes a fill value, rank, function, and input Shaped array.
-- Runs the function using the given rank (or the input array rank, whichever
-- is lower), using the given fill value if needed.
go1 :: a -> Int -> (Shaped a -> Shaped a) -> Shaped a -> Shaped a
go1 z mv v (Shaped shape xs) = homogenize z frame
  $ NE.fromList [v (Shaped rank $ slice (i*sz) sz xs) | i <- [0..product frame-1]]
  where
    (frame, rank) = splitAt (length shape - min mv (length shape)) shape
    sz = product rank

-- Two-argument variant of the above.
-- Takes a fill value, left and right rank, function, and two input Shaped
-- arrays.
go2 :: a -> Int -> Int -> (Shaped a -> Shaped a -> Shaped a) ->
  Shaped a -> Shaped a -> Shaped a
go2 z lv rv v (Shaped shapeX xs) (Shaped shapeY ys)
  | or $ zipWith (/=) frameX frameY = error "frame mismatch"
  | length frameX > length frameY =
    f (flip v) (frameY, rankY) ys (frameX, rankX) xs
  | otherwise =
    f        v (frameX, rankX) xs (frameY, rankY) ys
  where
    dimL = length shapeX - min lv (length shapeX)
    dimR = length shapeY - min rv (length shapeY)
    (frameX, rankX) = splitAt dimL shapeX
    (frameY, rankY) = splitAt dimR shapeY

    f v (frameX, rankX) xs (frameY, rankY) ys = homogenize z frameY $
       join $ NE.fromList [NE.fromList [v (Shaped rankX $ slice (i*xsize) xsize xs)
                  (Shaped rankY $ slice ((i*m + j)*ysize) ysize ys)
                | j <- [0..m-1] ] | i <- [0..product frameX-1]]
      where
        xsize = product rankX
        ysize = product rankY
        m = div (NE.length ys * xsize) (NE.length xs * ysize)

--test1 :: (a -> a) -> Shaped a -> Shaped a
--test1 f s0 = pure (f $ extract s0)

--test2 :: (a -> a -> a) -> Shaped a -> Shaped a -> Shaped a
--test2 f s0 s1 = pure (f (extract s0) (extract s1))

replicate' :: Int -> a -> NonEmpty a
replicate' n x = NE.fromList (replicate n x)

--{-
----hDyad :: (Int -> Int -> Int) -> Noun -> Noun -> Noun
--hDyad op x@(NounInt (Shaped shapeX _)) y@(NounInt (Shaped shapeY _))
--  | or $ zipWith (/=) shapeX shapeY = undefined
--  | length shapeX > length shapeY   = hDyad' (flip op) y x
--  | otherwise                       = hDyad'       op  x y

--hDyad' op (NounInt (Shaped shapeX xs)) (NounInt (Shaped shapeY ys)) =
--  let dy = div (NE.length ys) $ product shapeX
--  in NounInt $ Shaped shapeY (V.zipWith op (joinMap (replicate' dy) xs) ys)
---}
