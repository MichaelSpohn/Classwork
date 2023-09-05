-- CSci 119, Lab 4
-- Michael Spohn

import Data.List (sort, stripPrefix) -- for your solution to Lab 3
import Control.Monad (replicateM)    -- for strings function at the end


---------------- Code provided to you in Lab 3 ----------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Length-Ordered Lists over "character type" a (aka "strings")
-- Invariant: In LOL n xs, n == length xs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is sorted with no duplicates
type Lang a = [LOL a]

-- Smart constructor for (finite) languages
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

---- Regular expressions, along with input and output
data RegExp = Empty                -- Empty language
            | Let Char             -- Single letter language
            | Union RegExp RegExp  -- Union
            | Cat RegExp RegExp    -- Concatenation
            | Star RegExp          -- Kleene star
            deriving (Show, Eq)

-- Compact display form for RegExp
newtype Compact = Compact RegExp

instance (Show Compact) where    -- use precedence to minimize parentheses
  showsPrec d (Compact r) = sp d r where
    sp d Empty         = showString "@"
    sp d (Let c)       = showString [c]
    sp d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                         sp 6 r1 .
                         showString "+" .
                         sp 6 r2
    sp d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                         sp 7 r1 .
                         sp 7 r2
    sp d (Star r1)     = sp 9 r1 .     -- prec(Star) = 8
                         showString "*"

-- Quick and dirty postfix RegExp parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = go w [] where
  go [] [r]              = r
  go ('+':xs) (r2:r1:rs) = go xs (Union r1 r2:rs)
  go ('.':xs) (r2:r1:rs) = go xs (Cat r1 r2:rs)
  go ('*':xs) (r:rs)     = go xs (Star r:rs)
  go ('@':xs) rs         = go xs (Empty:rs)
  go (x:xs) rs           = go xs (Let x:rs)


---------------- Your solution to Lab 3 ----------------

-- Include here any code you need from your solution to Lab 3 for testing
-- purposes. After a few days, I will release my solution for you to use.
-- Don't duplicate the code just given above.

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL a xs) (LOL b ys) = LOL (a + b) (xs ++ ys)

-- Membership for languages (infinite lists satisfying invariant included)
memb :: Ord a => LOL a -> Lang a -> Bool
memb _ [] = False
memb x (l:ls) = case compare x l of LT -> False
                                    EQ -> True
                                    GT -> memb x ls

-- Merge of langages (aka "union")
merge :: Ord a => Lang a -> Lang a -> Lang a
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = case compare x y of LT -> x : merge xs (y:ys)
                                          EQ -> x : merge xs ys
                                          GT -> y : merge (x:xs) ys

-- Concatenation of languages
cat :: Ord a => Lang a -> Lang a -> Lang a
cat _ [] = []
cat [] _ = []
cat (x:xs) (y:ys) = dot x y : merge (cat [x] ys) (cat xs (y:ys))

-- Kleene star of languages
kstar :: Ord a => Lang a -> Lang a
kstar [] = [eps]
kstar (LOL 0 []:xr) = kstar xr 
kstar xs = eps : cat xs (kstar xs)

-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty       = []
lang_of (Let a)     = lang [[a]]
lang_of (Union a b) = merge (lang_of a) (lang_of b)
lang_of (Cat a b)   = cat (lang_of a) (lang_of b)
lang_of (Star a)    = kstar (lang_of a)



---------------- Part 1 ----------------

-- Implement the seven recursive predicates/operations on RegExp given in
-- Section 3.3 of the notes; call them empty, unitary, byp, inf, rever, lq,
-- and nep. Each should begin with a type declaration. Include several tests
-- for each function.

empty :: RegExp -> Bool
empty Empty         = True
empty (Let a)       = False
empty (Union r1 r2) = empty r1 && empty r2
empty (Cat r1 r2)   = empty r1 || empty r2
empty (Star r)      = False
-- *Main> empty Empty
-- True
-- *Main> empty (Let 'a')
-- False
-- *Main> empty (Union Empty (Cat (Let 'a') (Let 'b')))
-- False
-- *Main> empty (Cat Empty (Cat (Let 'a') (Let 'b')))  
-- True
-- *Main> empty (Star (Let 'a'))
-- False

unitary :: RegExp -> Bool
unitary Empty         = False
unitary (Let a)       = False
--unitary (Union r1 r2) = ((r1 == (Star Empty)) && (empty r2)) || 
  --                      ((empty r1) && (r2 == (Star Empty))) ||
    --                    ((r1 == (Star Empty)) && (r2 == (Star Empty)))
unitary (Union r1 r2) = (unitary r1 && empty r2) || (empty r1 && unitary r2) || (unitary r1 && unitary r2)
--unitary (Cat r1 r2)   = (r1 == (Star Empty)) && (r2 == (Star Empty))
unitary (Cat r1 r2)   = (unitary r1) && (unitary r2)
--unitary (Star r)      = (empty r) || (r == (Star Empty))
unitary (Star r)      = (empty r) || (unitary r)
-- *Main> unitary Empty 
-- False
-- *Main> unitary (Let 'a')
-- False
-- *Main> unitary (Union Empty Empty)
-- False
-- *Main> unitary (Union (Star Empty) Empty)
-- True
-- *Main> unitary (Union Empty (Let 'a'))   
-- False
-- *Main> unitary (Cat (Star Empty) (Star Empty))
-- True
-- *Main> unitary (Cat (Star Empty) Empty)       
-- False
-- *Main> unitary (Star (Let 'a'))
-- False
-- *Main> unitary (Star Empty)
-- True
-- *Main> unitary (Star (Star Empty))
-- True

byp :: RegExp -> Bool
byp Empty         = False
byp (Let a)       = False
byp (Union r1 r2) = (byp r1) || (byp r2)
byp (Cat r1 r2)   = (byp r1) && (byp r2)
byp (Star r)      = True
-- *Main> toRE "b*a.ab+*."    
-- Cat (Cat (Star (Let 'b')) (Let 'a')) (Star (Union (Let 'a') (Let 'b')))
-- *Main> r1 = it 
-- *Main> take 10 (lang_of r1)
-- ["a","aa","ab","ba","aaa","aab","aba","abb","baa","bab"]
-- *Main> r2 = Star (Let 'a')
-- *Main> take 10 (lang_of r2)
-- ["","a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa"]
-- *Main> r3 = Cat (Star (Union (Let 'a') (Cat (Let 'b') (Let 'c')))) (Let 'a')
-- *Main> take 10 (lang_of r3)
-- ["a","aa","aaa","bca","aaaa","abca","bcaa","aaaaa","aabca","abcaa"]
-- *Main> r4 = Star (Union (Let 'a') (Cat (Let 'b') (Let 'c')))
-- *Main> take 10 (lang_of r4)
-- ["","a","aa","bc","aaa","abc","bca","aaaa","aabc","abca"]
-- *Main> r5 = Union (Cat (Let 'a') (Let 'b')) (Let 'c')
-- *Main> lang_of r5
-- ["c","ab"]

-- *Main> byp r1  
-- False
-- *Main> byp r2
-- True
-- *Main> byp (Let 'a')
-- False
-- *Main> byp (Union r1 r2)
-- True
-- *Main> byp (Union r1 (Let 'a'))
-- False
-- *Main> byp (Cat r1 r2)
-- False
-- *Main> byp (Cat r2 r4)
-- True
-- *Main> byp (Star r1)
-- True
-- *Main> byp (Star Empty)
-- True

inf :: RegExp -> Bool
inf Empty         = False
inf (Let a)       = False
inf (Union r1 r2) = (inf r1) || (inf r2)
inf (Cat r1 r2)   = ((inf r1) && not (empty r2)) || ((inf r2) && not (empty r1))
inf (Star r)      = (not (empty r) && not (unitary r))
-- *Main> inf r1
-- True
-- *Main> inf r2    
-- True
-- *Main> inf (Let 'a')
-- False
-- *Main> inf (Union (Let 'a') r3)
-- True
-- *Main> inf (Union (Star Empty) (Let 'a'))
-- False  
-- *Main> inf (Cat r1 r3)
-- True   
-- *Main> inf (Cat r4 (Let 'a'))
-- True   
-- *Main> inf (Cat (Star Empty) Empty)
-- False
-- *Main> inf (Star r5)
-- True   
-- *Main> inf (Let 'a')
-- False
-- *Main> inf (Star Empty)
-- False

rever :: RegExp -> RegExp
rever Empty         = Empty
rever (Let a)       = Let a
rever (Union r1 r2) = Union (rever r1) (rever r2)
rever (Cat r1 r2)   = Cat (rever r2) (rever r1)
rever (Star r)      = Star (rever r)
-- *Main> rever Empty
-- Empty  
-- *Main> rever (Let 'a')
-- Let 'a'
-- *Main> rever (Union (Let 'a') (Let 'b'))
-- Union (Let 'a') (Let 'b')
-- *Main> rever (Union (Cat (Let 'a') (Let 'b')) (Cat (Let 'c') (Let 'd')))
-- Union (Cat (Let 'b') (Let 'a')) (Cat (Let 'd') (Let 'c'))
-- *Main> lang_of it
-- ["ba","dc"]
-- *Main> rever r1
-- Cat (Star (Union (Let 'a') (Let 'b'))) (Cat (Let 'a') (Star (Let 
-- 'b')))
-- *Main> rever r2
-- Star (Let 'a')
-- *Main> rever r3
-- Cat (Let 'a') (Star (Union (Let 'a') (Cat (Let 'c') (Let 'b'))))
-- *Main> rever r4
-- Star (Union (Let 'a') (Cat (Let 'c') (Let 'b')))

lq :: Char -> RegExp -> RegExp
lq x Empty         = Empty
lq x (Let a)       | x == a    = (Star Empty)
                   | otherwise = Empty
lq x (Union r1 r2) = Union (lq x r1) (lq x r2)
lq x (Cat r1 r2)   | byp r1    = Union (Cat (lq x r1) r2) (lq x r2)
                   | otherwise = Cat (lq x r1) r2
lq x (Star r)      = Cat (lq x r) (Star r)
-- *Main> lq 'a' r1
-- Cat (Union (Cat (Cat Empty (Star (Let 'b'))) (Let 'a')) (Star Empty)) (Star (Union (Let 'a') (Let 'b')))
-- *Main> lq 'a' r2
-- Cat (Star Empty) (Star (Let 'a'))
-- *Main> lq 'b' r3
-- Union (Cat (Cat (Union Empty (Cat (Star Empty) (Let 'c'))) (Star 
-- (Union (Let 'a') (Cat (Let 'b') (Let 'c'))))) (Let 'a')) Empty   
-- *Main> lq 'a' r4
-- Cat (Union (Star Empty) (Cat Empty (Let 'c'))) (Star (Union (Let 
-- 'a') (Cat (Let 'b') (Let 'c'))))

nep :: RegExp -> RegExp
nep Empty         = Empty
nep (Let a)       = Let a
nep (Union r1 r2) = Union (nep r1) (nep r2)
nep (Cat r1 r2)   | byp r1    = Union (Cat (nep r1) r2) (nep r2)
                  | otherwise = Cat (nep r1) r2
nep (Star r)      = Cat (nep r) (Star r)


---------------- Part 2 ----------------

-- Implement the two matching algorithms given in Section 3.4 of the notes,
-- where the second algorithm is the modified one I posted on Piazza (@96).
-- Call them match1 and match2. Start by implementing splits:

-- splits xs = list of all possible splits of xs, in order. For example,
-- splits "abc" = [("","abc"), ("a","bc"), ("ab","c"), ("abc","")]
splits :: [a] -> [([a], [a])]
splits [] = [([],[])]
splits xs = splits_h xs [] where
  splits_h :: [a] -> [a] -> [([a], [a])]
  splits_h [] ys     = [(ys,[])]
  splits_h (x:xs) [] = ([],(x:xs)) : splits_h xs [x]
  splits_h (x:xs) ys = (ys,(x:xs)) : splits_h xs (ys ++ [x])


match1 :: RegExp -> String -> Bool
match1 Empty w         = False
match1 (Let a) w       = elem a w && ((length w) == 1)
match1 (Union r1 r2) w = (match1 r1 w) || (match1 r1 w)
match1 (Cat r1 r2) w   = or [(match1 r1 w1) && (match1 r2 w2) | (w1, w2) <- splits w]
match1 (Star r) w      | w == []   = True
                       | otherwise = or [((match1 r w1) && (match1 (Star r) w2)) | (w1, w2) <- tail (splits w)]


match2 :: RegExp -> String -> Bool
match2 r w = match2h [r] w where
  match2h :: [RegExp] -> String -> Bool
  match2h [] w                 | length w == 0 = True
                               | otherwise = False
  match2h (Empty:rs) w         = False
  match2h ((Let a):rs) w       = (w == (a : (tail w))) && (match2h rs (tail w))
  match2h ((Union r1 r2):rs) w = (match2h (r1:rs) w) || (match2h (r2:rs) w)
  match2h ((Cat r1 r2):rs) w   = match2h (r1:r2:rs) w
  match2h ((Star r):rs) w      = (match2h rs w) || if byp(r) then match2h ((nep r):(Star r):rs) w else match2h (r:(Star r):rs) w


-- Some regular expressions for testing. Also, test them on other solutions
-- to the exercises of Section 3.2 (as many as you can get). 

sigma = ['a', 'b']                -- Alphabet used in all examples below

ab   = toRE "aa.bb.+*"            -- every letter is duplicated
ttla = toRE "ab+*a.ab+.ab+."      -- third to last letter is a
ena  = toRE "b*a.b*.a.*b*."       -- even number of a's
bb1  = toRE "aba.+*b.b.aab.+*."   -- contains bb exactly once


-- For your tests, you may also find the following helpful. For example,
-- you can generate all strings of length 10 (or 20) or less and then test
-- to see whether match1 r w == memb (lol w) (lang_of r) for each such w.

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

