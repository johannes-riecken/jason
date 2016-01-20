import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec

-- Experiments with ';:' in a J interpreter suggest the following.
jLine :: Parser [String]
jLine = (map unwords . groupBy ((. isJNum) . (&&) . isJNum)) -- Join numbers.
  <$> (spaces >> many jToken)  -- Eat leading spaces.

isJNum s@(c:_) = (isDigit c || c == '_') && last s `notElem` ".:"

jToken = (string "NB." >>= (<$> many anyChar) . (++)) <|> ((++) <$>  -- NB.
  (many1 (char '_' <|> alphaNum) <|> count 1 anyChar)  -- e.g. "ab_12" or "#".
  <*> many (oneOf ".:")                                -- e.g. "..:.:.::.".
  >>= (spaces >>) . return)                            -- Eat trailing spaces.

data Fragment = Noun String
              | Verb (String, String)
              | Adverb ((String, Int), (String, Int))
              | Conjunction
              | Copula
              | LParen
              | RParen
              | Cash
              deriving Show

jFind xs = let ws = words xs in case length ws of
  1 | all isDigit xs                  -> Noun (show (read xs :: Int))
    | xs `Map.member` dict            -> dict Map.! xs
    | otherwise                       -> Cash
  _ | all (all isDigit) ws            -> Noun (show (read <$> ws :: [Int]))
    | otherwise                       -> Cash
  where
    dict = Map.fromList
      [ ("#", Verb ("length", "replicate"))
      , ("+", Verb ("TODO", "(+)"))
      , ("-", Verb ("negate", "(-)"))
      , ("*", Verb ("signum", "(*)"))
      , ("%", Verb ("(1/) . fromIntegral",
                    "(\\x y -> fromIntegral x / fromIntegral y)"))
      , (">:", Verb ("(1+)", "(fromEnum .) . (>)"))
      , ("i.", Verb ("flip take [0..]",
                     "(\\x y -> case elemIndex y x of " ++
                     "{Just i -> i; n -> length x})"))
      , ("/", Adverb (("foldr1", 2), ("flip . (map .) . flip . (map .)", 2)))
      , ("\\", Adverb (("(. tail. inits) . map", 1), ("TODO", 2)))
      , ("(", LParen)
      , (")", RParen)
      ]

main = interact $ \input -> unlines $ (<$> lines input) $ \s -> let
  Right ws = parse jLine "" s
  xs = Cash : reverse (Cash:(jFind <$> filter (not . isPrefixOf "NB.") ws))
  in case run xs [] of
    [Cash, Noun s, Cash] -> s
    _ -> "syntax error: " ++ show xs

push     [] st = st
push (x:xs) st = run xs (x:st)

-- http://www.jsoftware.com/help/jforc/parsing_and_execution_ii.htm
run xs st
  | length st < 4 = push xs st
  -- 0 Monad
  | ccl,    (Verb (v, _), Noun n)         <- (x1, x2) =
    run xs (x0:Noun (concat [v, " $ ", n]):x3:rest)
  -- 1 Monad
  | cclavn, (Verb _, Verb (v, _), Noun n) <- (x1, x2, x3) =
    run xs (x0:x1:Noun (concat [v, " $ ", n]):rest)
  -- 2 Dyad
  | cclavn, (Noun m, Verb (_, v), Noun n) <- (x1, x2, x3) =
    run xs (x0:Noun (concat ["(", v, " $ ", m, ") $ ", n]):rest)
  -- 3 Adverb
  | cclavn, (Verb vs, Adverb (a1, a2))    <- (x1, x2) = let
    adverb (a, 1) (v, _) = concat ["(", a, " $ ", v, ")"]
    adverb (a, 2) (_, v) = concat ["(", a, " $ ", v, ")"]
    in run xs (x0:Verb (adverb a1 vs, adverb a2 vs):x3:rest)
  -- 5 Fork
  | cclavn, (Verb (u1, u2), Verb (v1, v2), Verb (w1, w2)) <- (x1, x2, x3) =
    run xs (x0:Verb (concat ["(", v2, " <$> ", u1, " <*> ", w1, ")"], "TODO"):rest)
  -- 8 Paren
  | LParen <- x0, isCAVN x1, RParen <- x2 = run xs (x1:x3:rest)
  | otherwise = push xs st
  where
    (x0:x1:x2:x3:rest) = st
    ccl = isCCL x0
    cclavn = ccl || isAVN x0

isCCL Cash   = True
isCCL Copula = True
isCCL LParen = True
isCCL _      = False

isAVN (Adverb _) = True
isAVN (Verb _)   = True
isAVN (Noun _)   = True
isAVN _          = False

isCAVN Conjunction = True
isCAVN x           = isAVN x
