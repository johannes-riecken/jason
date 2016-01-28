import Control.Monad
import Jumble
import Shaped
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tree
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V
import Data.Vector ((!))
import System.IO

jLine :: Parser [String]
jLine = (map unwords . groupBy ((. isJNum) . (&&) . isJNum)) -- Join numbers.
  <$> (spaces >> many jToken)  -- Eat leading spaces.

isJNum s@(c:_) = (isDigit c || c == '_') && last s `notElem` ".:"

jToken =
    ( (string "NB." >>= (<$> many anyChar) . (++)) -- NB.
  <|> do
    char '\''
    s <- concat <$> many (many1 (noneOf "'") <|> try (string "''"))
    char '\'' <?> "closing quote"
    return $ concat ["'", s, "'"]
  <|> ((++) <$> (many1 (char '_' <|> alphaNum) <|> count 1 anyChar)
  <*> many (oneOf ".:")) -- e.g. "ab_12" or "#" followed by e.g. "..:.:.::.".
    ) >>= (spaces >>) . return -- Eat trailing spaces.

instance Show Fragment where
  show (Noun n) = show n
  show _ = "TODO"

data JMonad = JMonad Int (Shaped Jumble -> Shaped Jumble)
data JDyad  = JDyad Int Int (Shaped Jumble -> Shaped Jumble -> Shaped Jumble)

data Fragment = Bad
              | Noun (Shaped Jumble)
              | Verb (JMonad, JDyad)
              | Adverb ((JMonad, JDyad) -> (JMonad, JDyad))
              | Conjunction ((JMonad, JDyad) -> (JMonad, JDyad) -> (JMonad, JDyad))
              | Copula
              | LParen
              | RParen
              | Edge

main = repl M.empty

repl dict = do
  putStr "   "
  hFlush stdout
  done <- isEOF
  unless done $ do
    s <- getLine
    let (out, dict') = eval dict s
    unless (isNothing out) $ putStrLn $ fromJust out
    repl dict'

eval dict s = case parse jLine "" s of
  Left err -> (Just $ "| " ++ show err, dict)
  Right ws -> let
    xs = "" : reverse ("":filter (not . isPrefixOf "NB.") ws)
    in run True dict xs []

jZero = intToJumble 0
verb1 (JMonad mu u, _)   = go1 jZero mu u
verb2 (_, JDyad lu ru u) = go2 jZero lu ru u

-- Shortcut for J monads of rank 0 that output atoms.
atomic1 f = JMonad 0  $ \(Shaped [] xs)
  -> singleton $ f (xs!0)

-- Shortcut for J dyads of rank 0 0 that output atoms.
atomic2  f = JDyad 0 0 $ \(Shaped [] xs) (Shaped [] ys)
  -> singleton $ f (xs!0) (ys!0)

vocab = M.fromList
  [ ("+:", Verb (atomic1 $ join jAdd, undefined))
  , ("*:", Verb (atomic1 $ join jMul, undefined))
  , ("+", Verb (undefined, atomic2 jAdd))
  , ("-", Verb (atomic1 $ jSub $ intToJumble 0, atomic2 jSub))
  , ("*", Verb (atomic1 $ intToJumble . signum . jumbleToInt, atomic2 jMul))
  , ("%", Verb (atomic1 $ jDiv (intToJumble 1), atomic2 jDiv))
  , ("^", Verb (atomic1 jExp, atomic2 jPow))
  , ("^.", Verb (atomic1 jLog, undefined))
  , ("%:", Verb (atomic1 jSqrt, undefined))
  , ("<", Verb (JMonad maxBound $ \x -> singleton $ jBox x, atomic2 jLT))
  , ("<.", Verb (atomic1 jFloor, atomic2 jMin))
  , (">", Verb (JMonad 0 $ \(Shaped [] x) -> jOpen (x!0), atomic2 jGT))
  , (">:", Verb (atomic1 $ jAdd (intToJumble 1), atomic2 jGE))
  , ("=", Verb (undefined, atomic2 jEQ))
  , ("|", Verb (atomic1 jMag, atomic2 jRes))
  , ("#:", Verb (JMonad maxBound undefined, JDyad 1 0 jAntibase))
  , ("I.", Verb (JMonad 1 jIndices, JDyad maxBound maxBound undefined))
  , ("/:", Verb (undefined, JDyad maxBound maxBound jSortUp))
  , ("{.", Verb (JMonad maxBound jHead, undefined))
  , (",", Verb
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped [product rs] xs
      , undefined
      ))
  , (",:", Verb
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped (1:rs) xs
      , JDyad maxBound maxBound $ \x y -> post [2] [x, y]
      ))
  , ("#", Verb
      ( JMonad maxBound $ \(Shaped rs _) -> singleton $ intToJumble $ head rs
      , JDyad 1 maxBound jCopy
      ))
  , ("i.", Verb
      ( JMonad 1 $ \(Shaped _ xs) -> let ns = jumbleToInt <$> V.toList xs
          in shapeList ns $ intToJumble <$> [0..product ns - 1]
      , undefined
      ))
  , ("j.", Verb
      ( atomic1 $ jMul jImaginary
      , atomic2 $ \x y -> jAdd x (jMul jImaginary y)
      ))
  , ("x:", Verb
      ( JMonad 1 $ \(Shaped rs xs) -> Shaped rs $ jExtend <$> xs
      , undefined
      ))
  , ("$", Verb
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped [length rs] $ V.fromList $ intToJumble <$> rs
      , undefined
      ))
  , ("/", Adverb $ \v@(_, JDyad lu ru op) ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> foldl1' (verb2 v) [Shaped rest $ V.slice (i*sz) sz xs | i <- [0..r-1], let sz = product rest]
      , JDyad lu maxBound $ go2 jZero lu ru op
      ))
  , ("\\", Adverb $ \v ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> post [r] $ map (verb1 v) $ zipWith (\i xs -> Shaped (i:rest) xs) [1..r] [V.slice 0 (i*sz) xs | i <- [1..r], let sz = product rest]
      , undefined
      ))
  , ("/.", Adverb $ \v -> (undefined, JDyad maxBound maxBound $ jKey v))
  , ("~", Adverb $ \v@(_, JDyad ru lu _) ->
    ( JMonad maxBound $ \x -> verb2 v x x
    , JDyad ru lu $ \x y -> verb2 v y x
    ))
  , ("=:", Copula)
  , ("=.", Copula)
  , ("(", LParen)
  , (")", RParen)
  , ("@", Conjunction jAtop)
  , ("&", Conjunction jCompose)
  ]

jHead x@(Shaped rrs xs)
  | null rrs     = x
  | r == 0 = x
  | otherwise    = Shaped rs $ V.slice 0 sz xs
  where
    (r:rs) = rrs
    sz = product rs

jKey v x y = let ((_, p), (_:ss, q)) = listsFrom x y
  in post [length $ nubFast p] $ map (\ks -> verb1 v $ Shaped (length ks:ss) $ V.concat $ map (q!!) ks) $ (`elemIndices` p) <$> nubFast p

nubFast xs = reverse $ f [] xs S.empty
  where
    f acc []     _    = acc
    f acc (x:xs) seen
      | x `S.member` seen = f acc xs seen
      | otherwise         = f (x:acc) xs (S.insert x seen)

jSortUp x@(Shaped rrs _) y = let ((_, p), (_, q)) = listsFrom x y
  in Shaped rrs $ V.concat $ snd <$> sortOn fst (zip q p)

listsFrom (Shaped rrs xs) (Shaped sss ys)
  | r /= s = error "length error"
  | otherwise = ((r:rs, p), (s:ss, q))
  where
    (r:rs) = if null rrs then 1:rrs else rrs
    (s:ss) = if null sss then 1:sss else sss
    p = [V.slice (i*sz) sz xs | i <- [0..r-1]] where sz = product rs
    q = [V.slice (i*sz) sz ys | i <- [0..s-1]] where sz = product ss

jIndices x@(Shaped rs xs) = let
  v =  V.concat [V.replicate (jumbleToInt $ xs!i) (intToJumble i) | i <- [0..V.length xs - 1]]
  in Shaped [V.length v] v

jAntibase :: Shaped Jumble -> Shaped Jumble -> Shaped Jumble
jAntibase (Shaped rs xs) y@(Shaped [] ys) = Shaped [V.length v] v
  where
    v = V.fromList $ intToJumble <$> f [] ms (jumbleToInt $ ys!0)
    ms = reverse $ jumbleToInt <$> V.toList xs
    f acc [] _ = acc
    f acc (m:ms) n = let (q, r) = divMod n m
      in f (r:acc) ms q

jCopy x@(Shaped rrs xs) y@(Shaped sss ys)
  | null rrs && null sss = jCopy (Shaped [1] xs) (Shaped [1] ys)
  | null rrs = let k = jumbleToInt (xs!0) in Shaped (s * k:ss) $ V.concat $ replicate k ys
  | null sss = let k = V.sum $ jumbleToInt <$> xs in Shaped [k] $ V.replicate k $ ys!0
  | r /= s = error "length error"
  | otherwise = let
    k = V.sum $ jumbleToInt <$> xs
    sz = product ss
    in Shaped (k:ss) $ V.concat [V.concat $ replicate (jumbleToInt $ xs!i) $ V.slice (i*sz) sz ys | i <- [0..s-1]]
  where
    (s:ss) = sss
    (r:rs) = rrs

jAtop u v@(JMonad mv _, _) =
  ( JMonad mv $ verb1 u . verb1 v
  , JDyad mv mv $ (verb1 u .) .  verb2 v
  )

jCompose u v@(JMonad mv _, _) =
  ( JMonad mv $ verb1 u . verb1 v
  , JDyad mv mv (verb2 u `on` verb1 v)
  )

jFork u v w =
  ( JMonad maxBound $ verb2 v <$> verb1 u <*> verb1 w
  , JDyad maxBound maxBound $ \x y -> verb2 v (verb2 u x y) (verb2 w x y)
  )

jHook u v =
  ( JMonad maxBound $ \y -> verb2 u y (verb1 v y)
  , JDyad maxBound maxBound $ \x y -> verb2 u x (verb1 v y)
  )

isName = all isAlpha

jFind :: M.Map String Fragment -> String -> Fragment
jFind dict s
  | null s = Edge
  | length ws > 1 = maybe Bad (Noun . fromList) $ mapM readJumble ws
  | Just jum <- readJumble s = Noun $ singleton jum
  | s `M.member` vocab = vocab M.! s
  | isName s = fromMaybe Bad (M.lookup s dict)
  | otherwise = LParen
  where ws = words s

run :: Bool -> M.Map String Fragment -> [String] -> [(Jumble, Fragment)] -> (Maybe String, M.Map String Fragment)
run echo dict xs st
  | length st < 4 = shift
  -- 0 Monad
  | cl,    (Verb v, Noun n)         <- (f1, f2) =
    reduce (x0:makeNoun (verb1 v n):x3:rest)
  -- 1 Monad
  | clavn, (Verb _, Verb v, Noun n) <- (f1, f2, f3) =
    reduce (x0:x1:makeNoun (verb1 v n):rest)
  -- 2 Dyad
  | clavn, (Noun m, Verb v, Noun n) <- (f1, f2, f3) =
    reduce (x0:makeNoun (verb2 v m n):rest)
  -- 3 Adverb
  | clavn, (Verb v, Adverb a)       <- (f1, f2) =
    reduce (x0:(jBox $ Shaped [2] $ V.fromList [j2, jBox $ singleton j1], Verb $ a v):x3:rest)
  -- 4 Conjunction
  | clavn, (Verb u, Conjunction c, Verb v) <- (f1, f2, f3) =
    reduce (x0:(jBox $ Shaped [2] $ V.fromList [j2, jBox $ Shaped [2] $ V.fromList [j1, j3]], Verb $ c u v):rest)
  -- 5 Fork
  | clavn, (Verb u, Verb v, Verb w) <- (f1, f2, f3) =
    reduce (x0:(jBox $ Shaped [2] $ V.fromList [jBox $ singleton $ intToJumble 3, jBox $ Shaped [3] $ V.fromList [j1, j2, j3]], Verb $ jFork u v w):rest)
  -- 6 Hook
  | cl,    (Verb u, Verb v) <- (f1, f2) =
    reduce (x0:(jBox $ Shaped [2] $ V.fromList [jBox $ singleton $ intToJumble 2, jBox $ Shaped [2] $ V.fromList [j1, j2]], Verb $ jHook u v):x3:rest)
  -- 7 Is
  | Just name <- jGets j0, Copula <- f1, isCAVN f2 =
    run False (M.insert name f2 dict) xs (x2:x3:rest)
  -- 8 Paren
  | LParen <- f0, isCAVN f1, RParen <- f2 = reduce (x1:x3:rest)
  | otherwise = shift
  where
    makeNoun x = (jBox x, Noun x)
    (x0:x1:x2:x3:rest) = st
    (j0:j1:j2:j3:_) = fst <$> st
    (f0:f1:f2:f3:_) = snd <$> st
    cl = isCL f0
    clavn = cl || isAVN f0
    reduce              = run True dict xs
    shift | (h:t) <- xs = run echo dict t $ (jPuts h, jFind dict h):st
          | otherwise   = (out, dict)
    out   | [_, _]    <- st = Nothing
          | [_, x, _] <- st = if echo then Just $ show $ snd x else Nothing
          | otherwise       = Just $ "syntax error: " ++ show st

    isCL Edge = True
    isCL Copula = True
    isCL LParen = True
    isCL _      = False

    isAVN (Adverb _) = True
    isAVN (Verb _)   = True
    isAVN (Noun _)   = True
    isAVN _          = False

    isCAVN (Conjunction _) = True
    isCAVN x               = isAVN x
