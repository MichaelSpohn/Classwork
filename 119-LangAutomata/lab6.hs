-- Lab 6:  FSM constructions for regular operators
-- Michael Spohn

import Data.List
import Data.Array
import Control.Monad (replicateM)  -- for strings function below

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- All strings on sigma of length <= n (useful for testing)
strings :: Int -> [String]
strings n = concat $ [replicateM i sigma | i <- [0..n]]

-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)


---------------- Your solution to Lab 5, ported to FSM a -------------------

-- Function I made back in lab2 to remove duplicates from a list.
remove_duplicates :: Eq a => [a] -> [a]
remove_duplicates [] = []
remove_duplicates (x:[]) = [x]
remove_duplicates (x:xs)
    | elem x xs = remove_duplicates xs
    | otherwise = x:remove_duplicates xs

-- Used for checkFSM by using remove_duplicates to show whether qs is unique.
unique_list :: Eq a => [a] -> Bool
unique_list xs = length (remove_duplicates xs) == length xs

checkFSM :: Eq a => FSM a -> Bool
checkFSM (qs, s, fs, d) = (unique_list qs) && (elem s qs) && (and [elem x qs | x <- fs]) && 
                          (and [elem (d x y) qs | x <- qs, y <- sigma])

-- Gives the delta* function (recursive in w)
dstar :: FSM a -> a -> [Char] -> a
dstar m q [] = q
dstar (m@(qs, s, fs, d)) q (w:ws) = dstar m (d q w) ws

-- Machine acceptance, Definition 1 (via delta*)
accept1 :: Eq a => FSM a -> [Char] -> Bool
accept1 (m@(qs, s, fs, d)) w = elem (dstar m s w) fs

-- Machine acceptance, Definition 2 (via L_q(M))
accept2 :: FSM Int -> [Char] -> Bool
accept2 (qs, s, fs, d) w = aux s w where
  -- aux q w = whether the machine, starting in q, accepts w (recursive in w)
  aux :: Int -> [Char] -> Bool
  aux q [] = elem q fs
  aux q (w:ws) = aux (d q w) ws

---------------- Some additional useful functions --------------------------

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

-- Cartesian product (preserves normalization)
(><) :: [a] -> [b] -> [(a,b)]
xs >< ys = [(x,y) | x <- xs, y <- ys]

-- Powerset  (preserves normalization)
power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = let ys = power xs
               in [] : map (x:) ys ++ tail ys

-- Check whether two lists overlap
overlap :: Eq a => [a] -> [a] -> Bool
overlap [] ys = False
overlap (x:xs) ys = elem x ys || overlap xs ys


---------------- Lab 6 begins here -----------------------------------------

-- Complete the FSM constructions for the regular expression operators
-- and test your functions adquately

oddbs :: FSM Int
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

-- Machine that accepts the empty language
emptyFSM :: FSM Int
emptyFSM = ([0], 0, [], d) where d _ _ = 0

-- Machine that accepts the language {"a"} where a in sigma
letterFSM :: Char -> FSM Int
letterFSM a = ([0,1,2], 0, [1], d) where d q x | q == 0 && x == a = 1
                                               | otherwise        = 2

-- Machine that accepts the union of the languages accepted by m1 and m2
unionFSM :: (Eq a, Eq b) => FSM a -> FSM b -> FSM (a, b)
unionFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< qs2
  s  = (s1,s2)
  fs = [(q1,q2) | (q1,q2) <- qs, elem q1 fs1 || elem q2 fs2]
  d (q1,q2) a = ((d1 q1 a), (d2 q2 a))

-- Machine that accepts the concatenation of the languages accepted by m1 and m2
catFSM :: (Eq a, Ord b) => FSM a -> FSM b -> FSM (a, [b])
catFSM (qs1, s1, fs1, d1) (qs2, s2, fs2, d2) = (qs, s, fs, d) where
  qs = qs1 >< power qs2
  s  = (s1, [s2 | elem s1 fs1])
  fs = [(q,x) | (q,x) <- qs, overlap x fs2]
  d (q,x) a = (q', [d2 qs2 a | qs2 <- xq]) where
    xq = if elem q fs1 then union x [s2] else x
    q' = d1 q a

xc :: Ord a => [a] -> [a] -> a -> [a]
xc x f s | overlap x f = norm (s : x)
         | otherwise   = norm x

-- Machine that accepts the Kleene star of the language accepted by m1
starFSM :: Ord a => FSM a -> FSM [a]
starFSM (qs1, s1, fs1, d1) = (qs, s, fs, d) where
  qs = norm [xc x fs1 s1 | x <- power qs1]
  s  = []
  fs = [] : [x | x <- qs, overlap x fs1]
  d x a | x == []   = xc ((d1 s1 a) : x) fs1 s1
        | otherwise = xc [d1 q a | q <- x] fs1 s1


---------------- Bonus Features (for testing and experimenting) ------------

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr

-- reachable m == the part of m that is reachable from the start state
reachable :: Ord a => FSM a -> FSM a
reachable m@(qs, s, fs, d) = (qs', s, fs', d) where
  qs' = uclosure [s] (\q -> map (d q) sigma)
  fs' = filter (`elem` fs) qs'

-- Change the states of an FSM from an equality type to Int 
-- and use an array lookup for the transition function
intify :: Eq a => FSM a -> FSM Int
intify (qs, s, fs, d) = ([0..n-1], s', fs', d') where
  n = length qs
  m = length sigma
  s'  = ind qs s
  fs' = map (ind qs) fs
  arr = listArray ((0,0), (n-1,m-1)) [ind qs (d q a) | q <- qs, a <- sigma]
  d' q a = arr ! (q, ind sigma a)
  ind (q':qs) q = if q == q' then 0 else 1 + ind qs q

reduce :: Ord a => FSM a -> FSM Int
reduce = intify . reachable

---- Regular expressions, along with output and input
data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp

instance (Show RegExp) where    -- use precedence to minimize parentheses
  showsPrec d Empty         = showString "@"
  showsPrec d (Let c)    = showString [c]
  showsPrec d (Union r1 r2) = showParen (d > 6) $  -- prec(Union) = 6
                              showsPrec 6 r1 .
                              showString "+" .
                              showsPrec 6 r2
  showsPrec d (Cat r1 r2)   = showParen (d > 7) $  -- prec(Cat) = 7
                              showsPrec 7 r1 .
                              showsPrec 7 r2
  showsPrec d (Star r1)     = showsPrec 9 r1 .     -- prec(Star) = 8
                              showString "*"

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = toRE' w [] where
  toRE' [] [r] = r
  toRE' ('+':xs) (r2:r1:rs) = toRE' xs (Union r1 r2:rs)
  toRE' ('.':xs) (r2:r1:rs) = toRE' xs (Cat r1 r2:rs)
  toRE' ('*':xs) (r:rs) = toRE' xs (Star r:rs)
  toRE' ('@':xs) rs = toRE' xs (Empty:rs)
  toRE' (x:xs) rs = toRE' xs (Let x:rs)

-- Use constructions above to get reduced machine associated with regex
-- Warning: it can take a lot of time/memory to compute these for "big" regex's
-- We will see much better ways later in the course
re2fsm :: RegExp -> FSM Int
re2fsm Empty = emptyFSM
re2fsm (Let c) = letterFSM c
re2fsm (Union r1 r2) = reduce $ unionFSM (re2fsm r1) (re2fsm r2)
re2fsm (Cat r1 r2) = reduce $ catFSM (re2fsm r1) (re2fsm r2)
re2fsm (Star r1) = reduce $ starFSM (re2fsm r1)


m1 :: FSM Int
m1 = ([0,1,2], 0, [2], d) where
  d 0 'a' = 1 ; d 0 'b' = 0
  d 1 'a' = 2 ; d 1 'b' = 0
  d 2 'a' = 2 ; d 2 'b' = 1

m2 :: FSM Int
m2 = ([3,4,5], 3, [5], d) where
  d 3 'a' = 1 ; d 3 'b' = 0
  d 4 'a' = 2 ; d 4 'b' = 1
  d 5 'a' = 2 ; d 5 'b' = 0

avoid_bbb :: FSM Int
avoid_bbb = ([1,2,3,4], 1, [1,2,3], d) where
  d 1 'a' = 1 ; d 1 'b' = 2
  d 2 'a' = 1 ; d 2 'b' = 3
  d 3 'a' = 1 ; d 3 'b' = 4
  d 4 'a' = 4 ; d 4 'b' = 4