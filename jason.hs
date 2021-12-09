import Jumble
import Shaped
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Vector ((!))
import qualified Data.Vector as V
import System.IO
import Text.ParserCombinators.Parsec
import Data.Function.Pointless
import Safe

type Noun = Shaped Jumble
data JMonad = JMonad Int (Noun -> Noun)
data JDyad  = JDyad Int Int (Noun -> Noun -> Noun)
type Dict = M.Map String Jumble

jLine :: Parser [String]
jLine = map unwords . groupBy ((&&) $:: isJNum ~> isJNum ~> id) -- Join numbers.
  <$> (spaces *> many jToken)  -- Eat leading spaces.

isJNum s = fromMaybe False $ (&&) <$> (((||) <$> isDigit <*> (== '_')) <$> headMay s) <*> ((`notElem` ".:") <$> lastMay s)

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

main = repl M.empty

repl dict = do
  hFlush stdout
  done <- isEOF
  unless done $ do
    s <- getLine
    let (out, dict') = eval dict s
    unless (isNothing out) $ putStrLn $ fromJust out
    repl dict'

eval :: Dict -> String -> (Maybe String, Dict)
eval dict s = case parse jLine "" s of
  Left err -> (Just $ "| " ++ show err, dict)
  Right ws -> let
    xs = "" : reverse ("":filter (not . isPrefixOf "NB.") ws)
    (mj, dict') = ast True dict xs []
    in (dump <$> mj, dict')

dump j = let Shaped _ xs = jOpen j in case jGetI $ xs!0 of
  Just 0  -> show $ jOpen $ xs!1
  Nothing -> show j

jZero = intToJumble 0
verb1 (JMonad mu u, _)   = go1 jZero mu u
verb2 (_, JDyad lu ru u) = go2 jZero lu ru u

-- Shortcut for J monads of rank 0 that output atoms.
atomic1 f = JMonad 0  $ \(Shaped [] xs)
  -> pure $ f (xs!0)

-- Shortcut for J dyads of rank 0 0 that output atoms.
atomic2 f = JDyad 0 0 $ \(Shaped [] xs) (Shaped [] ys)
  -> pure $ f (xs!0) (ys!0)

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
  , (">", (JMonad 0 $ \(Shaped [] x) -> jOpen (x!0), atomic2 jGT))
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
      , JDyad maxBound maxBound $ \x y -> post [2] [x, y]
      ))
  , ("#",
      ( JMonad maxBound $ \(Shaped rs _) -> pure $ intToJumble $ head rs
      , JDyad 1 maxBound jCopy
      ))
  , ("i.",
      ( JMonad 1 $ \(Shaped _ xs) -> let ns = jumbleToInt <$> V.toList xs
          in shapeList ns $ intToJumble <$> [0..product ns - 1]
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
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped [length rs] $ V.fromList $ intToJumble <$> rs
      , undefined
      ))
  ]

adverbDict = M.fromList
  [ ("/", \v@(_, JDyad lu ru op) ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> foldl1' (verb2 v) [Shaped rest $ V.slice (i * sz) sz xs |
         let sz = product rest, i <- [0 .. r - 1]]
      , JDyad lu maxBound $ go2 jZero lu ru op
      ))
  , ("\\", \v ->
    ( JMonad maxBound $ \x@(Shaped rs xs) -> case rs of
      [] -> x
      (r:rest) -> post [r] $ map (verb1 v) $ zipWith (\i xs -> Shaped (i:rest) xs) [1..r] [V.slice 0 (i * sz) xs | let sz = product rest, i <- [1 .. r]]
      , undefined
      ))
  , ("/.", \v -> (undefined, JDyad maxBound maxBound $ jKey v))
  , ("~", \v@(_, JDyad ru lu _) ->
    ( JMonad maxBound $ \x -> verb2 v x x
    , JDyad ru lu $ \x y -> verb2 v y x
    ))
  ]

conjunctionDict = M.fromList
  [ ("&", (undefined, jBondM, jBondN, jCompose))
  , ("@", (undefined, undefined, undefined, jAtop))
  , ("`", (undefined, undefined, undefined, undefined))
  , ("@.", (undefined, undefined, undefined, undefined))
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

jAntibase :: Noun -> Noun -> Noun
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
    reduce (j0:run dict (jBox $ fromList [j1, jBox $ pure j2]):j3:rest)
  -- 1 Monad
  | eclavn, isV j1, isV j2, isN j3 =
    reduce (j0:j1:run dict (jBox $ fromList [j2, jBox $ pure j3]):rest)
  -- 2 Dyad
  | eclavn, isN j1, isV j2, isN j3 =
    reduce (j0:run dict (jBox $ fromList [j2, jBox $ fromList [j1, j3]]):rest)
  -- 3 Adverb
  | eclavn, isV j1, isA j2 =
    reduce (j0:jBox (fromList [j2, jBox $ pure j1]):j3:rest)
  -- 4 Conjunction
  | eclavn, isV j1 || isN j1, isC j2, isV j3 || isN j3 =
    reduce (j0:jBox (fromList [j2, jBox $ fromList [j1, j3]]):rest)
  -- 5 Fork
  | eclavn, isV j1, isV j2, isV j3 =
    reduce (j0:jBox (fromList [jBox $ pure $ intToJumble 3, jBox $ fromList [j1, j2, j3]]):rest)
  -- 6 Hook
  | ecl, isV j1, isV j2 =
    reduce (j0:jBox (fromList [jBox $ pure $ intToJumble 2, jBox $ fromList [j1, j2]]):j3:rest)
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
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ xs!0 = i == 4
          | otherwise = False
    isV j | Just x <- f j = isV x
          | Just s <- jGets j = s `M.member` verbDict
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ xs!0 = i == 2 || i == 3
          | Shaped [2] xs <- jOpen j = isA (xs!0) || (isC (xs!0) && Just "`" /= jGets (xs!0))
          | otherwise = False
    isN j | Just x <- f j = isN x
          | Shaped [2] xs <- jOpen j, Just i <- jGetI $ xs!0 = i == 0
          | Shaped [2] xs <- jOpen j = isV (xs!0) || Just "`" == jGets (xs!0)
          | otherwise = False
    isC j | Just x <- f j = isC x
          | Just s <- jGets j = s `M.member` conjunctionDict
          | otherwise = False
    isCAVN j = isC j || isA j || isV j || isN j
    match j ss | Just s <- jGets j = s `elem` ss
               | otherwise = False

    encNoun x = jBox $ fromList [jBox $ pure $ intToJumble 0, jBox x]
    shift | (h:t) <- xs     = ast echo dict t $ atomize h:st
          | otherwise       = (out, dict)
    out   | not echo        = Nothing
          | [_, _]    <- st = Nothing
          | [_, x, _] <- st = Just x
          | otherwise       = Just $ jPuts $ "|syntax error: " ++ show st
    reduce = ast True dict xs

atomize s
  | null s = jPuts ""
  | length ws > 1 = maybe (jPuts "|syntax error") (tag 0 . fromList) $ mapM readJumble ws
  | Just j <- readJumble s = tag 0 $ pure j
  | otherwise = jPuts s
  where ws = words s

sym dict j | Just s <- jGets j, isName s = M.lookup s dict
           | otherwise = Nothing

run :: Dict -> Jumble -> Jumble
run dict j
  | Just x <- sym dict j = x
  | null rs = j
  | Just i <- jGetI $ xs!0 = case i of
    0 -> j
  | Just "`" <- jGets $ xs!0 = jTie dict (args!0) (args!1)
  | Just v <- verbOf dict' $ xs!0 =
    case V.length args of
    1 -> let y = nounOf $ run dict' (args!0) in tag 0 $ verb1 v y
    2 -> let
      x = nounOf $ run dict' (args!0)
      y = nounOf $ run dict' (args!1)
      in tag 0 $ verb2 v x y
  where
    Shaped rs xs = jOpen j
    Just word = jGets $ xs!0
    Shaped _ args = jOpen $ xs!1
    dict' = M.insertWith (\ _ x -> x) "$:" (xs!0) dict

verbOf dict j
  | Just s <- jGets j, s == "$:" = Just $ recur dict $ dict M.! "$:"
  | Just s <- jGets j, Just v <- M.lookup s verbDict = Just v
  | Just s <- jGets $ xs!0, Just a <- M.lookup s adverbDict = let Just v = verbOf dict $ args!0 in Just $ a v
  | Just s <- jGets $ xs!0, s /= "`", Just c <- M.lookup s conjunctionDict = Just $ if s == "@." then runAgenda dict (args!0) (args!1) else runConjunction dict c (args!0) (args!1)
  | Just i <- jGetI $ xs!0, i == 2 = let
    [Just u, Just v] = verbOf dict <$> V.toList args
    in Just $ jHook u v
  | Just i <- jGetI $ xs!0, i == 3 = let
    [Just u, Just v, Just w] = verbOf dict <$> V.toList args
    in Just $ jFork u v w
  | otherwise = Nothing
  where
    Shaped rs xs = jOpen j
    Shaped _ args = jOpen $ xs!1

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
tag i m = jBox $ fromList [jBox $ pure $ intToJumble i, jBox m]

nounOf j = let Shaped _ xs = jOpen j in jOpen $ xs!1

isName = all isAlpha

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
  | [Nothing,  Just _] <- verbOf dict <$> [j0, j1] = tag 0 $ Shaped [r+1] $ V.snoc xs j1
  | [Just _ , Nothing] <- verbOf dict <$> [j0, j1] = tag 0 $ Shaped [s+1] $ V.cons j0 ys
  | [Just _ ,  Just _] <- verbOf dict <$> [j0, j1] = tag 0 $ fromList [j0, j1]
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
  | null ss = fromJust $ verbOf dict $ xs!jumbleToInt (ys!0)
  | otherwise = error "TODO: agenda trains"

agendaM dict m@(Shaped rs xs) v@(JMonad mv _, JDyad lv rv _) =
  ( JMonad mv $ \n -> verb1 (agenda dict m $ verb1 v n) n
  , JDyad lv rv $ \m n -> verb2 (agenda dict m $ verb2 v m n) m n
  )
