import Control.Monad
import Jumble
import Shaped
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import qualified Data.Vector as V
import Data.Vector ((!))
import System.IO

-- Parser.
jLine :: Parser [String]
jLine = (map unwords . groupBy ((. isJNum) . (&&) . isJNum)) -- Join numbers.
  <$> (spaces >> many jToken)  -- Eat leading spaces.

isJNum s@(c:_) = (isDigit c || c == '_') && last s `notElem` ".:"

jToken = (string "NB." >>= (<$> many anyChar) . (++)) <|> ((++) <$>  -- NB.
  (many1 (char '_' <|> alphaNum) <|> count 1 anyChar)  -- e.g. "ab_12" or "#".
  <*> many (oneOf ".:")                                -- e.g. "..:.:.::.".
  >>= (spaces >>) . return)                            -- Eat trailing spaces.

instance Show Fragment where
  show (Noun n) = show n
  show _ = "TODO"

data Fragment = Bad
              | Unknown
              | Noun (Shaped Jumble)
              | Verb (JMonad, JDyad)
              | Adverb ((JMonad, JDyad) -> (JMonad, JDyad))
              | Conjunction
              | Copula
              | LParen
              | RParen

isName = all isAlpha

jFind :: M.Map String Fragment -> String -> Fragment
jFind dict s
  | length ws > 1 = maybe Bad (Noun . fromList) $ mapM readJumble ws
  | Just jum <- readJumble s = Noun $ singleton jum
  | s `M.member` vocab = vocab M.! s
  | isName s = fromMaybe Unknown (M.lookup s dict)
  where ws = words s

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

eval dict s = let
  Right ws = parse jLine "" s
  xs = "" : reverse ("":filter (not . isPrefixOf "NB.") ws)
  in run True dict (zip xs $ repeat Unknown) []

data JMonad = JMonad Int (Shaped Jumble -> Shaped Jumble)
data JDyad  = JDyad Int Int (Shaped Jumble -> Shaped Jumble -> Shaped Jumble)

jZero = intToJumble 0
verb1 (JMonad r op, _)    = go1 jZero r op
verb2 (_, JDyad rL rR op) = go2 jZero rL rR op

-- Shortcut for J monads of rank 0 that output atoms.
atomicMonad f = JMonad 0  $ \(Shaped [] xs)
  -> singleton $ f (xs!0)

-- Shortcut for J dyads of rank 0 0 that output atoms.
atomicDyad  f = JDyad 0 0 $ \(Shaped [] xs) (Shaped [] ys)
  -> singleton $ f (xs!0) (ys!0)

vocab = M.fromList
  [ ("+:", Verb
      ( atomicMonad $ join jAdd
      , undefined
      ))
  , ("*:", Verb
      ( atomicMonad $ join jMul
      , undefined
      ))
  , ("+", Verb
      ( undefined
      , atomicDyad jAdd
      ))
  , ("-", Verb
      ( atomicMonad $ jSub (intToJumble 0)
      , atomicDyad jSub
      ))
  , ("*", Verb
      ( atomicMonad $ intToJumble . signum . jumbleToInt
      , atomicDyad jMul
      ))
  , ("%", Verb
      ( atomicMonad $ jDiv (intToJumble 1)
      , atomicDyad jDiv
      ))
  , ("<", Verb
      ( JMonad maxBound $ \arg -> Shaped [] $ V.singleton $ jBox arg
      , atomicDyad jLT
      ))
  , (">", Verb
      ( undefined
      , atomicDyad jGT
      ))
  , (">:", Verb
      ( atomicMonad $ jAdd (intToJumble 1)
      , atomicDyad jGE
      ))
  , (",:", Verb
      ( JMonad maxBound $ \(Shaped rs xs) -> Shaped (1:rs) xs
      , JDyad maxBound maxBound $ \x y -> post [2] [x, y]
      ))
  , ("#", Verb
      ( JMonad maxBound $ \(Shaped rs _) -> singleton $ intToJumble $ head rs
      , undefined
      ))
  , ("/:", Verb
      ( undefined
      , JDyad maxBound maxBound jSortUp
      ))
  , ("i.", Verb
      ( JMonad 1 $ \(Shaped _ xs) -> let ns = jumbleToInt <$> V.toList xs
          in shapeList ns $ intToJumble <$> [0..product ns - 1]
      , undefined
      ))
  , ("x:", Verb
      ( JMonad 1 $ \(Shaped rs xs) -> Shaped rs $ jExtend <$> xs
      , undefined
      ))
  , ("/", Adverb $ \v ->
    ( JMonad maxBound $ \arg@(Shaped rs xs) -> case rs of
      [] -> arg
      (r:rest) -> foldl1' (verb2 v) [Shaped rest $ V.slice (i*sz) sz xs | i <- [0..r-1], let sz = product rest]
      , undefined
      ))
  , ("\\", Adverb $ \v ->
    ( JMonad maxBound $ \arg@(Shaped rs xs) -> case rs of
      [] -> arg
      (r:rest) -> post [r] $ map (verb1 v) $ zipWith (\i xs -> Shaped (i:rest) xs) [1..r] [V.slice 0 (i*sz) xs | i <- [1..r], let sz = product rest]
      , undefined
      ))
  , ("~", Adverb $ \v@(_, JDyad ru lu _) ->
    ( JMonad maxBound $ \arg -> verb2 v arg arg
    , JDyad ru lu $ \x y -> verb2 v y x
    ))
  , ("/.", Adverb $ \v ->
    ( undefined
    , JDyad maxBound maxBound $ jKey v
    ))
  , ("=:", Copula)
  , ("=.", Copula)
  , ("(", LParen)
  , (")", RParen)
  ]

jKey v x y = let ((_, p), (_:ss, q)) = listsFrom x y
  in post [length $ nub p] $ map (\ks -> verb1 v $ Shaped (length ks:ss) $ V.concat $ map (q!!) ks) $ (`elemIndices` p) <$> nub p

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

-- http://www.jsoftware.com/help/jforc/parsing_and_execution_ii.htm
run :: Bool -> M.Map String Fragment -> [(String, Fragment)] -> [(String, Fragment)] -> (Maybe String, M.Map String Fragment)
run echo dict xs st
  | length st < 4 = shift
  -- 0 Monad
  | ccl,    (Verb v, Noun n)         <- (f1, f2) =
    reduce (x0:makeNoun (verb1 v n):x3:rest)
  -- 1 Monad
  | cclavn, (Verb _, Verb v, Noun n) <- (f1, f2, f3) =
    reduce (x0:x1:makeNoun (verb1 v n):rest)
  -- 2 Dyad
  | cclavn, (Noun m, Verb v, Noun n) <- (f1, f2, f3) =
    reduce (x0:makeNoun (verb2 v m n):rest)
  -- 3 Adverb
  | cclavn, (Verb v, Adverb a)       <- (f1, f2) =
    reduce (x0:makeVerb (a v):x3:rest)
  -- 7 Is
  | isName $ fst x0, Copula <- f1, isCAVN f2 =
    run False (M.insert (fst x0) f2 dict) xs (x2:x3:rest)
  -- 8 Paren
  | LParen <- f0, isCAVN f1, RParen <- f2 = reduce (x1:x3:rest)
  | otherwise = shift
  where
    makeNoun x = (show x, Noun x)
    makeVerb x = ("", Verb x)
    (x0:x1:x2:x3:rest) = st
    (f0:f1:f2:f3:_) = toFragment <$> st
    toFragment (s, f)
      | Unknown <- f = jFind dict s
      | otherwise    = f
    ccl = null (fst x0) || isCL f0
    cclavn = ccl || isAVN f0
    reduce              = run True dict xs
    shift | (h:t) <- xs = run echo dict t $ h:st
          | otherwise   = (out, dict)
    out   | [_, x, _] <- st = if echo then Just $ show $ toFragment x else Nothing
          | otherwise       = Just $ "syntax error: " ++ show (fst <$> st)

    isCL Copula = True
    isCL LParen = True
    isCL _      = False

    isAVN (Adverb _) = True
    isAVN (Verb _)   = True
    isAVN (Noun _)   = True
    isAVN _          = False

    isCAVN Conjunction = True
    isCAVN x           = isAVN x
