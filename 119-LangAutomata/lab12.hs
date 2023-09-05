import Data.List (foldl')
import Data.Char (isUpper)

-- CFG G = (Start, Follows)
type CFG = (Char, Char -> Char -> [String])

accept :: CFG -> [Char] -> Bool
accept (s,d) = elem [] . foldl' (\xs c -> concatMap (lq c) xs) [[s]] where
  lq a [] = []
  lq a (x:xs) | isUpper x = map (++ xs) $ d x a          -- nonterminal
              | otherwise = if a == x then [xs] else []  -- terminal

-- Example 1: Balanced parentheses (not including the empty string)
-- original: S --> () | (S) | SS
-- in TNF:   S --> () | ()S | (S) | (S)S
bp :: CFG
bp = ('S', d) where
  d 'S' '(' = [")", ")S", "S)", "S)S"]
  d 'S' ')' = []

-- Example 2: {a^i b^j c^{i+j} | i >= 0, j > 0}
-- original: S --> aSc | T
--           T --> bTc | bc
-- in TNF:   S --> aSc | bTc | bc
--           T --> bTc | bc
pl = ('S', d) where
  d 'S' 'a' = ["Sc"]  ;  d 'S' 'b' = ["Tc","c"]  ;  d 'S' 'c' = []
  d 'T' 'a' = []      ;  d 'T' 'b' = ["Tc","c"]  ;  d 'T' 'c' = []

-- G5
-- original: S --> aSb | bSa | SS | ε
-- in TNF:   S --> aSb | bSa | aSbS | bSaS | ab | ba
g5 = ('S', d) where
  d 'S' 'a' = ["Sb", "SbS", "S", "b"]
  d 'S' 'b' = ["Sa", "S", "SaS", "a"]

-- G6
-- original: S --> bS | Sa | aSb | ε
-- in TNF:   S --> bS | aSb | a | b | ab | bST | aSbT | bT | aT | abT
--           T --> aT | a
g6 = ('S', d) where
  d 'S' 'a' = ["Sb", "", "b", "SbT", "T", "bT"]  ;  d 'T' 'a' = ["T", ""]
  d 'S' 'b' = ["S", "", "ST", "T"]               ;  d 'T' 'b' = []

-- G2
-- original: S --> T + S | S
--           T --> TU | U
--           U --> U* | V | (S)
--           V --> 0 | 1 | ε | a | b
-- in TNF:   S --> a | b | aT | bT | (T) | (S)T | 0 | 1 | 0T | 1T
--           T --> a | b | +T | * | aT | bT | *T | (S) | (S)T | 0 | 1 | 0T | 1T
g2 = ('S', d) where
  d 'S' 'a' = ["T", ""]      ;  d 'T' 'a' = ["T", ""]
  d 'S' 'b' = ["T", ""]      ;  d 'T' 'b' = ["T", ""]
  d 'S' '(' = ["T)", "S)T"]  ;  d 'T' '(' = ["S)", "S)T"]
  d 'S' ')' = ["T"]          ;  d 'T' ')' = ["T"]
  d 'S' '+' = []             ;  d 'T' '+' = ["T"]
  d 'S' '*' = []             ;  d 'T' '*' = ["T", ""]
  d 'S' '0' = ["T", ""]      ;  d 'T' '0' = ["T", ""]
  d 'S' '1' = ["T", ""]      ;  d 'T' '1' = ["T", ""]

-- Examples
-- *Main> accept g2 "ab*(ba+b+bb)*+aba+a"
-- True
-- *Main> accept g2 "a+b+a+bbbb"
-- True
-- *Main> accept g2 "aaaaaaaaa"
-- True
-- *Main> accept g2 "a(b(a(b*****)))"
-- True
-- *Main> accept g2 ""
-- False
-- *Main> accept g2 "abba+aa**b(a+bb)*+abb*+1"  
-- True
-- *Main> accept g5 "ab"
-- True
-- *Main> accept g5 "abbbbaabaa"
-- True
-- *Main> accept g5 ""
-- False
-- *Main> accept g5 "a"
-- False
-- *Main> accept g5 "aaaaabbbbbaaaaabbbbbababababab"
-- True
-- *Main> accept g6 ""
-- False  
-- *Main> accept g6 "a"
-- True   
-- *Main> accept g6 "b"
-- True   
-- *Main> accept g6 "ababababa"
-- True   
-- *Main> accept g6 "aaaaaaaaaaaaaaaaaa"
-- True
-- *Main> accept g6 "bbbbbbbbbbbbbbbbbb"
-- True   
-- *Main> accept g6 "aaaabbbabababaaababab"
-- True