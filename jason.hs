import Jumble
import Shaped
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as V
import System.IO
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Safe
import Data.List.Extra
import Text.Printf
import Debug.Trace
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Comonad
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

type Noun = Shaped Jumble
data JMonad = JMonad Int (Noun -> Noun)
data JDyad  = JDyad Int Int (Noun -> Noun -> Noun)
type Dict = M.Map String Jumble

-- >>> parseTest jLine "i. 2 3 4"
-- ["i.","2 3 4"]
jLine :: Parser [String]
jLine = fmap unwords . groupBy ((&&) `on` isJNum) -- Join numbers.
  <$> (spaces *> many jToken)  -- Eat leading spaces.

isJNum :: String -> Bool
isJNum s = fromMaybe False $ (&&) <$> (((||) <$> isDigit <*> (== '_')) <$> headMay s) <*> ((`notElem` ".:") <$> lastMay s)

-- >>> parseTest jToken "i. 2 3 4"
-- "i."
jToken :: Parser String
jToken = (
    (string "NB." <> many anyChar)
    <|> (
        string "'" <>
        (concat <$> many (some (noneOf "'") <|> try (string "''"))) <>
        string "'")
    <|> (
        (some (char '_' <|> alphaNum) <|> count 1 anyChar) <>
        many (oneOf ".:"))
    ) <* spaces

main = void $ runMaybeT (repl M.empty)

repl :: Dict -> MaybeT IO String
repl dict = forever $ do
  liftIO $ hFlush stdout
  done <- liftIO isEOF
  guard $ not done
  s <- liftIO getLine
  let (out, dict') = eval dict s
  unless (isNothing out) $ liftIO $ putStrLn $ fromJust out

eval :: Dict -> String -> (Maybe String, Dict)
eval dict s = case parse jLine "" s of
  Left err -> (Just $ "| " ++ show err, dict)
  Right ws -> let
    xs = "" : reverse ("":filter (not . isPrefixOf "NB.") ws)
    -- (mj, dict') = trace (show $ ast True dict xs []) $ ast True dict xs []
    (mj, dict') = ast True dict xs []
    in (dump <$> mj, dict')

dump :: Jumble -> String
-- if the extract of the boxed data in j matches (I _), show the opened next
-- element, else show the entire j
dump j = let Shaped _ xs = jOpen j in case jGetI $ extract xs of
  Just 0  -> show $ jOpen $ xs NE.!! 1
  Nothing -> show j

jZero :: Jumble
jZero = intToJumble 0

verb1 :: (JMonad, a) -> Shaped Jumble -> Shaped Jumble
verb1 (JMonad mu u, _)   = go1 jZero mu u

verb2 :: (a, JDyad) -> Shaped Jumble -> Shaped Jumble -> Shaped Jumble
verb2 (_, JDyad lu ru u) = go2 jZero lu ru u

-- Shortcut for J monads of rank 0 that output atoms.
atomic1 :: (Jumble -> Jumble) -> JMonad
atomic1 f = JMonad 0  $ \(Shaped [] xs)
  -> pure $ f (extract xs)

-- Shortcut for J dyads of rank 0 0 that output atoms.
atomic2 :: (Jumble -> Jumble -> Jumble) -> JDyad
atomic2 f = JDyad 0 0 $ \(Shaped [] xs) (Shaped [] ys)
  -> pure $ f (extract xs) (extract ys)

verbDict :: M.Map String (JMonad, JDyad)
verbDict = M.fromList
  [ ("+:", (atomic1 $ join jAdd, undefined))
  , ("*:", (atomic1 $ join jMul, undefined))
  , ("-:", (atomic1 $ flip jDiv (intToJumble 2), undefined))
  , ("+", (undefined, atomic2 jAdd))
  , ("-", (atomic1 $ jSub $ intToJumble 0, atomic2 jSub))
  , ("*", (atomic1 $ intToJumble . signum . jumbleToInt, atomic2 jMul))
  , ("%", (atomic1 $ jDiv (intToJumble 1), atomic2 jDiv))
  , ("^", (atomic1 jExp, atomic2 jPow))
  , ("^.", (atomic1 jLog, undefined))
  , ("%:", (atomic1 jSqrt, undefined))
  , ("<", (JMonad maxBound $ \x -> pure $ jBox x, atomic2 jLT))
  , ("<.", (atomic1 jFloor, atomic2 jMin))
  , (">", (JMonad 0 $ \(Shaped [] x) -> jOpen (extract x), atomic2 jGT))
  , ("<:", (atomic1 $ jAdd (intToJumble (-1)), atomic2 jLE))
  , (">:", (atomic1 $ jAdd (intToJumble 1), atomic2 jGE))
  , ("=", (undefined, atomic2 jEQ))
  , ("[", (JMonad maxBound id, JDyad maxBound maxBound const))
  , ("]", (JMonad maxBound id, JDyad maxBound maxBound $ \ _ x -> x))
  , ("$:", (JMonad maxBound $ const $ pure $ jPuts "|stack error", JDyad maxBound maxBound $ \_ _ -> pure $ jPuts "|stack error"))
  , ("|", (atomic1 jMag, atomic2 jRes))
  , ("#:", (JMonad maxBound undefined, JDyad 1 0 jAntibase))
  , ("I.", (JMonad 1 jIndices, JDyad maxBound maxBound undefined))
  , ("/:", (undefined, JDyad maxBound maxBound jSortUp))
  , ("{.", (JMonad maxBound jHead, undefined))
  , ("1:", (JMonad maxBound $ const $ pure $ intToJumble 1,
            JDyad maxBound maxBound $ \_ _ -> pure $ intToJumble 1))
  , (",",
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped [product rs] xs
      , undefined
      ))
  , (",:",
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped (1:rs) xs
      , JDyad maxBound maxBound $ \x y -> post [2] (x :| [y])
      ))
  , ("#",
      ( JMonad maxBound $ \(Shaped rs _) -> pure $ intToJumble $ head rs
      , JDyad 1 maxBound jCopy
      ))
  , ("i.",
      ( JMonad 1 $ \(Shaped _ xs) -> let ns = jumbleToInt <$>  xs
          in shapeList (NE.toList ns) $ intToJumble <$> NE.fromList [0..product (fmap abs ns) - 1]
      , undefined
      ))
  , ("j.",
      ( atomic1 $ jMul jImaginary
      , atomic2 $ \x y -> jAdd x (jMul jImaginary y)
      ))
  , ("x:",
      ( JMonad 1 $ \(Shaped rs xs) -> Shaped rs $ jExtend <$> xs
      , undefined
      ))
  , ("$",
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped [length rs] $ intToJumble <$> NE.fromList rs
      , undefined
      ))
  ]

adverbDict :: M.Map String ((JMonad, JDyad) -> (JMonad, JDyad))
adverbDict = M.fromList
  [ ("/", \v@(_, JDyad lu ru op) ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> foldl1' (verb2 v) [Shaped rest $ slice (i * sz) sz xs |
         let sz = product rest, i <- [0 .. r - 1]]
      , JDyad lu maxBound $ go2 jZero lu ru op
      ))
  , ("\\", \v ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> post [r] $ fmap (verb1 v) $ NE.zipWith (\i xs -> Shaped (i:rest) xs) (NE.fromList [1..r]) (NE.fromList [slice 0 (i * sz) xs | let sz = product rest, i <- [1 .. r]])
      , undefined
      ))
  , ("/.", \v -> (undefined, JDyad maxBound maxBound $ jKey v))
  , ("~", \v@(_, JDyad ru lu _) ->
    ( JMonad maxBound $ \x -> verb2 v x x
    , JDyad ru lu $ \x y -> verb2 v y x
    ))
  ]

conjunctionDict :: M.Map
                        [Char]
                        (a, Shaped Jumble -> (a1, JDyad) -> (JMonad, JDyad),
                         (a2, JDyad) -> Shaped Jumble -> (JMonad, JDyad),
                         (JMonad, JDyad) -> (JMonad, JDyad) -> (JMonad, JDyad))
conjunctionDict = M.fromList
  [ ("&", (undefined, jBondM, jBondN, jCompose))
  , ("@", (undefined, undefined, undefined, jAtop))
  , ("`", (undefined, undefined, undefined, undefined))
  , ("@.", (undefined, undefined, undefined, undefined))
  ]

-- equivalent to monadic {.
jHead :: Shaped a -> Shaped a
jHead x@(Shaped rrs xs)
  | null rrs     = x
  | r == 0 = x
  | otherwise    = Shaped rs . NE.fromList $ NE.take sz xs
  where
    (r:rs) = rrs
    sz = product rs

-- equivalent to dyadic /.
jKey :: (JMonad, a) -> Shaped Jumble -> Shaped Jumble -> Shaped Jumble
jKey v x y = undefined
-- jKey v x y = let ((_, p), (_:ss, q)) = listsFrom x y
--   in post [length $ nubFast p] $ fmap (\ks -> verb1 v $ Shaped (V.length ks:ss) $ (=<<) (q V.!!) ks) $ (`elemIndices` p) <$> nubFast (fmap NE.toList p)

elemIndices' :: Eq a => a -> NonEmpty a -> [Int]
elemIndices' x xs = (x `elemIndices`) . NE.toList $ xs

-- nubFast :: [NonEmpty Jumble] -> [NonEmpty Jumble]
nubFast :: [[Jumble]] -> [[Jumble]]
-- nubFast xs = fmap NE.fromList $ reverse $ f [] xs' S.empty
nubFast xs = reverse $ f [] xs' S.empty
  where
    f acc []     _    = acc
    f acc (x:xs) seen
      | x `S.member` seen = f acc xs seen
      | otherwise         = f (x:acc) xs (S.insert x seen)
    -- xs' = fmap NE.toList xs
    xs' = xs

-- equivalent to dyadic /:
jSortUp x@(Shaped rrs _) y = let ((_, p), (_, q)) = listsFrom x y
  in Shaped rrs $ join $ snd <$> NE.sortWith fst (NE.fromList $ zip q p)

listsFrom :: Shaped a -> Shaped b -> (([Int], [NonEmpty a]), ([Int], [NonEmpty b]))
listsFrom (Shaped rrs xs) (Shaped sss ys)
  | r /= s = error "length error"
  | otherwise = ((r:rs, p), (s:ss, q))
  where
    (r:rs) = if null rrs then 1:rrs else rrs
    (s:ss) = if null sss then 1:sss else sss
    p = [slice (i*sz) sz xs | i <- [0..r-1]] where sz = product rs
    q = [slice (i*sz) sz ys | i <- [0..s-1]] where sz = product ss

-- equivalent to monadic I.
jIndices x@(Shaped rs xs) = let
  v =  join $ NE.fromList [replicate' (jumbleToInt $ xs NE.!! i) (intToJumble i) | i <- [0..length xs - 1]]
  in Shaped [NE.length v] v

-- equivalent to dyadic #:
jAntibase :: Noun -> Noun -> Noun
jAntibase (Shaped rs xs) y@(Shaped [] ys) = Shaped [NE.length v] v
  where
    v =  intToJumble <$> NE.fromList (f [] ms (jumbleToInt $ extract ys))
    ms = NE.reverse $ jumbleToInt <$>  xs
    f acc ms n = fst $ foldl' (\(acc,n) m -> let (q,r) = divMod n m in (r:acc,q)) (acc,n) ms

-- equivalent to dyadic #
jCopy x@(Shaped rrs xs) y@(Shaped sss ys)
  | null rrs && null sss = jCopy (Shaped [1] xs) (Shaped [1] ys)
  | null rrs = let k = jumbleToInt (extract xs) in Shaped (s * k:ss) $ join $ replicate' k ys
  | null sss = let k = V.sum $ jumbleToInt <$> xs in Shaped [k] $ replicate' k $ extract ys
  | r /= s = error "length error"
  | otherwise = let
    k = V.sum $ jumbleToInt <$> xs
    sz = product ss
    in Shaped (k:ss) $ join $ NE.fromList [join $ replicate' (jumbleToInt $ xs NE.!! i) $ slice (i*sz) sz ys | i <- [0..s-1]]
  where
    (s:ss) = sss
    (r:rs) = rrs

jFork u v w =
  ( JMonad maxBound $ verb2 v <$> verb1 u <*> verb1 w
  , JDyad maxBound maxBound $ \x y -> verb2 v (verb2 u x y) (verb2 w x y)
  )

jHook u v =
  ( JMonad maxBound $ \y -> verb2 u y (verb1 v y)
  , JDyad maxBound maxBound $ \x y -> verb2 u x (verb1 v y)
  )

ast :: Bool -> Dict -> [String] -> [Jumble] -> (Maybe Jumble, Dict)
ast echo dict xs st
  | length st < 4 = shift
  -- 0 Monad
  | ecl, isV j1, isN j2 =
    reduce (j0:run dict (jBox $ fromList (j1 :| [jBox $ pure j2])):j3:rest)
  -- 1 Monad
  | eclavn, isV j1, isV j2, isN j3 =
    reduce (j0:j1:run dict (jBox $ fromList (j2 :| [jBox $ pure j3])):rest)
  -- 2 Dyad
  | eclavn, isN j1, isV j2, isN j3 =
    reduce (j0:run dict (jBox $ fromList (j2 :| [jBox $ fromList (j1 :| [j3])])):rest)
  -- 3 Adverb
  | eclavn, isV j1, isA j2 =
    reduce (j0:jBox (fromList (j2 :| [jBox $ pure j1])):j3:rest)
  -- 4 Conjunction
  | eclavn, isV j1 || isN j1, isC j2, isV j3 || isN j3 =
    reduce (j0:jBox (fromList (j2 :| [jBox $ fromList (j1 :| [j3])])):rest)
  -- 5 Fork
  | eclavn, isV j1, isV j2, isV j3 =
    reduce (j0:jBox (fromList ((jBox $ pure $ intToJumble 3) :| [jBox $ fromList (j1 :| [j2, j3])])):rest)
  -- 6 Hook
  | ecl, isV j1, isV j2 =
    reduce (j0:jBox (fromList ((jBox $ pure $ intToJumble 2) :| [jBox $ fromList (j1 :| [j2])])):j3:rest)
  -- 7 Is
  | Just name <- jGets j0, match j1 ["=.", "=:"], isCAVN j2 =
    ast False (M.insert name j2 dict) xs (j2:j3:rest)
  -- 8 Paren
  | match j0 ["("], isCAVN j1, match j2 [")"] = reduce (j1:j3:rest)
  | otherwise = shift
  where
    (j0:j1:j2:j3:rest) = st
    ecl = match j0 ["", "=.", "=:", "("]
    eclavn = ecl || isA j0 || isV j0 || isN j0
    f = sym dict
    isA j | Just x <- f j = isA x
          | Just s <- jGets j = s `M.member` adverbDict
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ extract xs = i == 4
          | otherwise = False
    isV j | Just x <- f j = isV x
          | Just s <- jGets j = s `M.member` verbDict
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ extract xs = i == 2 || i == 3
          | Shaped [2] xs <- jOpen j = isA (extract xs) || (isC (extract xs) && Just "`" /= jGets (extract xs))
          | otherwise = False
    isN j | Just x <- f j = isN x
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ extract xs = i == 0
          | Shaped [2] xs <- jOpen j = isV (extract xs) || Just "`" == jGets (extract xs)
          | otherwise = False
    isC j | Just x <- f j = isC x
          | Just s <- jGets j = s `M.member` conjunctionDict
          | otherwise = False
    isCAVN j = isC j || isA j || isV j || isN j
    match j ss | Just s <- jGets j = s `elem` ss
               | otherwise = False

    encNoun x = jBox $ fromList ((jBox $ pure $ intToJumble 0) :| [jBox x])
    shift | (h:t) <- xs     = ast echo dict t $ atomize h:st
          | otherwise       = (out, dict)
    out   | not echo        = Nothing
          | [_, _]    <- st = Nothing
          | [_, x, _] <- st = Just x
          | otherwise       = Just $ jPuts $ "|syntax error: " ++ show st
    reduce = ast True dict xs

atomize :: String -> Jumble
atomize s
  | null s = jPuts ""
  | length ws > 1 = maybe (jPuts "|syntax error") (tag 0 . fromList) $ mapM readJumble (NE.fromList ws)
  | Just j <- readJumble s = tag 0 $ pure j
  | otherwise = jPuts s
  where ws = words s

sym :: M.Map String a -> Jumble -> Maybe a
sym dict j = jGets j >>= (`M.lookup` dict)

run :: Dict -> Jumble -> Jumble
run dict j
  | Just x <- sym dict j = x
  | null rs = j
  | Just i <- jGetI $ extract xs = case i of
      0 -> j
  | Just "`" <- jGets $ extract xs = jTie dict (extract args) (args NE.!! 1)
  | Just v <- verbOf dict' $ extract xs =
    case V.length args of
    1 -> let y = nounOf $ run dict' (extract args) in tag 0 $ verb1 v y
    2 -> let
      x = nounOf $ run dict' (extract args)
      y = nounOf $ run dict' (args NE.!! 1)
      in tag 0 $ verb2 v x y
  where
    Shaped rs xs = jOpen j
    Just word = jGets $ extract xs
    Shaped _ args = jOpen $ xs NE.!! 1
    dict' = M.insertWith (\ _ x -> x) "$:" (extract xs) dict

verbOf dict j
  | Just s <- jGets j, s == "$:" = Just $ recur dict $ dict M.! "$:"
  | Just s <- jGets j, Just v <- M.lookup s verbDict = Just v
  | Just s <- jGets $ extract xs, Just a <- M.lookup s adverbDict = let Just v = verbOf dict $ extract args in Just $ a v
  | Just s <- jGets $ extract xs, s /= "`", Just c <- M.lookup s conjunctionDict = Just $ if s == "@." then runAgenda dict (extract args) (args NE.!! 1) else runConjunction dict c (extract args) (args NE.!! 1)
  | Just i <- jGetI $ extract xs, i == 2 = let
      (Just u :| [Just v]) = verbOf dict <$>  args
      in Just $ jHook u v
  | Just i <- jGetI $ extract xs, i == 3 = let
      (Just u :| [Just v, Just w]) = verbOf dict <$>  args
      in Just $ jFork u v w
  | otherwise = Nothing
  where
    Shaped rs xs = jOpen j
    Shaped _ args = jOpen $ xs NE.!! 1

recur dict j = let v = fromJust $ verbOf dict j
  in (JMonad maxBound $ verb1 v, JDyad maxBound maxBound $ verb2 v)

runConjunction dict (nn, nv, vn, vv) j0 j1
  | [Nothing, Nothing] <- verbOf dict <$> [j0, j1] = nn m n
  | [Nothing,  Just v] <- verbOf dict <$> [j0, j1] = nv m v
  | [Just u , Nothing] <- verbOf dict <$> [j0, j1] = vn u n
  | [Just u ,  Just v] <- verbOf dict <$> [j0, j1] = vv u v
  where
    m = nounOf $ run dict j0
    n = nounOf $ run dict j1

tag :: Integral a => a  -> Noun -> Jumble
tag i m = jBox $ fromList ((jBox $ pure $ intToJumble i) :| [jBox m])

nounOf j = let Shaped _ xs = jOpen j in jOpen $ xs NE.!! 1

jAtop u v@(JMonad mv _, _) =
  (JMonad mv $ verb1 u . verb1 v, JDyad mv mv $ (verb1 u .) . verb2 v)

jCompose u v@(JMonad mv _, _) =
  (JMonad mv $ verb1 u . verb1 v, JDyad mv mv (verb2 u `on` verb1 v))

jBondM m v =
  (JMonad maxBound $ verb2 v m, JDyad 0 maxBound undefined)

jBondN u n =
  (JMonad maxBound $ flip (verb2 u) n, JDyad 0 maxBound undefined)

jTie dict j0 j1
  | [Nothing, Nothing] <- verbOf dict <$> [j0, j1] = undefined
  | [Nothing,  Just _] <- verbOf dict <$> [j0, j1] = tag 0 $ Shaped [r+1] $ snoc' xs j1
  | [Just _ , Nothing] <- verbOf dict <$> [j0, j1] = tag 0 $ Shaped [s+1] $ (<>) (pure j0) ys
  | [Just _ ,  Just _] <- verbOf dict <$> [j0, j1] = tag 0 $ fromList (j0 :| [j1])
  where
    Shaped [r] xs = nounOf $ run dict j0
    Shaped [s] ys = nounOf $ run dict j1

runAgenda dict j0 j1
  | [Nothing, Nothing] <- verbOf dict <$> [j0, j1] = agenda dict m n
  | [Nothing,  Just v] <- verbOf dict <$> [j0, j1] = agendaM dict m v
  | [Just u , Nothing] <- verbOf dict <$> [j0, j1] = undefined
  | [Just u ,  Just v] <- verbOf dict <$> [j0, j1] = undefined
  where
    m = nounOf $ run dict j0
    n = nounOf $ run dict j1

agenda dict (Shaped rs xs) (Shaped ss ys)
  | length rs > 1 || length ss > 1 = error "rank error"
  | null rs = agenda dict (Shaped [1] xs) (Shaped ss ys)
  | null ss = fromJust $ verbOf dict $ xs NE.!! jumbleToInt (extract ys)
  | otherwise = error "TODO: agenda trains"

agendaM dict m@(Shaped rs xs) v@(JMonad mv _, JDyad lv rv _) =
  ( JMonad mv $ \n -> verb1 (agenda dict m $ verb1 v n) n
  , JDyad lv rv $ \m n -> verb2 (agenda dict m $ verb2 v m n) m n
  )

snoc' :: NonEmpty a ->a -> NonEmpty a
snoc' xs x = xs <> pure x
