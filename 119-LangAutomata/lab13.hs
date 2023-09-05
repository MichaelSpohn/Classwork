import Data.List (foldl')
import Data.Char (isUpper)

-- CFG' G = (Start, Follows, Nullable)
type CFG' = (Char, Char -> Char -> [String], Char -> Bool)

close' :: String -> [String]
close' [] = []
close' (x:xs) | isUpper x = [(x:xs)] ++ close' xs
              | otherwise = []

accept' :: CFG' -> String -> Bool
accept' (s,d,e) =  elem [] . foldl' (\xs c -> concatMap close (concatMap (lq c) xs)) (close [s]) where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal
  close :: String -> [String]
  close [] = [""]
  close (x:xs) | isUpper x && e x = (x:xs) : close xs
               | otherwise = [x:xs]

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal

-- Balanced parentheses
-- Original:  S --> (S) | SS | ε
-- TNF + ε:   S --> (S) | (S)S  (S nullable)
bp' :: CFG'
bp' = ('S', d, e) where
  d 'S' '(' = ["S)", "S)S"]
  d 'S' ')' = []
  e 'S' = True
-- *Main> accept' bp' "((("
-- False  
-- *Main> accept' bp' "()" 
-- True
-- *Main> accept' bp' "((())())()"
-- True  

-- G5
-- original: S --> aSb | bSa | SS | ε
-- TNF + ε:  S --> aSb | bSa | aSbS | bSaS (S nullable)
g5' = ('S', d, e) where
  d 'S' 'a' = ["Sb", "SbS"]
  d 'S' 'b' = ["Sa", "SaS"]
  e 'S' = True
g5'' = ('S', d, e) where      -- more simplified
  d 'S' 'a' = ["SbS"]
  d 'S' 'b' = ["SaS"]
  e 'S' = True
-- *Main> accept' g5' "aaa"  
-- False  
-- *Main> accept' g5'' "aaa"
-- False  
-- *Main> accept' g5' "ababab"
-- True

-- G6
-- original: S --> bS | Sa | aSb | ε
-- in TNF:   S --> bS | aSb | bST | aSbT | bT | aT | abT (S nullable)
--           T --> aT (T nullable)
g6' = ('S', d, e) where
  d 'S' 'a' = ["Sb", "SbT", "T", "bT"]  ;  d 'T' 'a' = ["T"]
  d 'S' 'b' = ["S", "ST", "T"]          ;  d 'T' 'b' = []
  e 'S' = True                          ;  e 'T' = True