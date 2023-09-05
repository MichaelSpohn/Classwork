-- CSci 119, Lab 3
-- Michael Spohn

-- See https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
import Data.List (sort, stripPrefix)


---------------- General list functions

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product, preserves normalization
cart :: [a] -> [b] -> [(a,b)]
cart xs ys = [(x,y) | x <- xs, y <- ys]

-- Powerset, preserves normalization. Examples:
-- power [] = [[]]
-- power [1] = [[],[1]]
-- power [1,2] = [[],[1],[1,2],[2]]
-- power [1,2,3] = [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]]
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys


---------------- Length-ordered lists

-- Length-Ordered Lists over "character type" a (aka "strings over a")
-- Invariant: In LOL n xs, n == length xs
-- Note that automatically-derived Ord instance correctly orders LOLs
data LOL a = LOL Int [a] deriving (Eq,Ord)

instance Show a => Show (LOL a) where
  show (LOL n xs) = show xs

-- Empty list (epsilon)
eps :: LOL a
eps = LOL 0 []

-- Smart constructor for LOL a, establishes invariant
lol :: [a] -> LOL a
lol xs = LOL (length xs) xs

-- Concatenation of LOLs, preserves invariant
dot :: LOL a -> LOL a -> LOL a
dot (LOL a xs) (LOL b ys) = LOL (a + b) (xs ++ ys)

-- Reverse of LOLs, preserves invariant
rev :: LOL a -> LOL a
rev (LOL a xs) = LOL a (reverse xs)



---------------- Languages

-- Normalized lists of LOLs (aka "languages")
-- Invariant: xs :: Lang a implies xs is ordered with no duplicates
type Lang a = [LOL a]


-- Constructor for languages, establishes invariant
lang :: Ord a => [[a]] -> Lang a
lang xs = norm $ map lol xs

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

-- Left quotient of a language by an LOL (cf. Definition 2.16)
-- Hint: Use the stripPrefix function
leftq :: Ord a => LOL a -> Lang a -> Lang a
leftq xs [] = []
leftq (LOL a xs) ((LOL b ys):yr) = case stripPrefix xs ys of Nothing -> leftq (LOL a xs) yr
                                                             Just zs -> (LOL (a-b) zs) : leftq (LOL a xs) yr


---- Regular expressions and the languages they denote 
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


-- The language associated to a regular expression, i.e., [[r]]
lang_of :: RegExp -> Lang Char
lang_of Empty = []
lang_of (Let a) = lang [[a]]
lang_of (Union a b) = merge (lang_of a) (lang_of b)
lang_of (Cat a b) = cat (lang_of a) (lang_of b)
lang_of (Star a) = kstar (lang_of a)


-- The one-string and finite languages of Theorem 3.2. It should be the case
-- that, for any string w, lang_of (onestr w) == [w], and for any (finite) list
-- of (distinct, sorted) strings l, lang_of (finite l) == l.
onestr :: String -> RegExp
onestr [] = Empty
onestr (x:[]) = (Let x)
onestr (x:xs) = Cat (Let x) (onestr xs)

finite :: [String] -> RegExp
finite [] = Empty
finite ((x:xs):[]) = Cat (Let x) (onestr xs)
finite ((x:xs):xr) = Union (Cat (Let x) (onestr xs)) (finite xr)


-- Test all of the above operations extensively!   

-- *Main> onestr "abcdef"
-- Cat (Let 'a') (Cat (Let 'b') (Cat (Let 'c') (Cat (Let 'd') (Cat (Let 'e') (Let 'f')))))
-- *Main> finite ["abc", "def"]
-- Union (Cat (Let 'a') (Cat (Let 'b') (Let 'c'))) (Cat (Let 'd') (Cat (Let 'e') (Let 'f')))
-- *Main> lang_of it
-- ["abc","def"]

-- *Main> r1 = Star (Union (Let 'a') (Let 'b'))
-- *Main> l1 = lang_of r1
-- *Main> take 20 l1
-- ["","a","b","aa","ab","ba","bb","aaa","aab","aba","abb","baa","bab","bba","bbb","aaaa","aaab","aaba","aabb","abaa"]
-- *Main> r2 = Cat (Cat (Star (Let 'b')) (Let 'a')) r1
-- *Main> Compact r2
-- b*a(a+b)*
-- *Main> l2 = lang_of r2
-- *Main> take 15 l2
-- ["a","aa","ab","ba","aaa","aab","aba","abb","baa","bab","bba","aaaa","aaab","aaba","aabb"]

-- *Main> toRE "b*a.ab+*."
-- Cat (Cat (Star (Let 'b')) (Let 'a')) (Star (Union (Let 'a') (Let 'b')))
-- *Main> take 15 (lang_of it)
-- ["a","aa","ab","ba","aaa","aab","aba","abb","baa","bab","bba","aaaa","aaab","aaba","aabb"]

-- *Main> dot (lol "abc") (lol "def")
-- "abcdef"
-- *Main> rev (lol "abcdef")
-- "fedcba"

-- *Main> take 10 (kstar (lang ["a", "b"]))     
-- ["","a","b","aa","ab","ba","bb","aaa","aab","aba"]
-- *Main> l1 = kstar (lang ["a", "b"])          
-- *Main> memb (lol "aaaaa") l1
-- True   
-- *Main> memb (lol "aacaa") l1
-- False

-- *Main> merge (lang ["a", "abb", "abc", "bba"]) (lang ["a", "ab", "abcc"])
-- ["a","ab","abb","abc","bba","abcc"]
-- *Main> take 10 (merge (kstar (lang ["a", "b"])) (kstar (lang ["c", "ab"])))
-- ["","a","b","c","aa","ab","ba","bb","cc","aaa"]

-- *Main> cat (lang ["a", "abb", "abc", "bba"]) (lang ["a", "ab", "abcc"])    
-- ["aa","aab","abba","abca","bbaa","aabcc","abbab","abcab","bbaab","abbabcc","abcabcc","bbaabcc"]
-- *Main> take 10 (cat (kstar (lang ["a", "b"])) (kstar (lang ["c", "ab"])))  
-- ["","a","b","c","aa","ab","ac","ba","bb","bc"]