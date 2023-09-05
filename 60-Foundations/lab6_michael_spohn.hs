-- Lab 6: Tracing and Boolean warm-up
-- Michael Spohn 110360841

import Debug.Trace (trace)


---- Functions from earlier labs ----------------------------------------------

-- Infinite list of primes
primes = 2 : primesFrom3 where 
    primesFrom3 = sieve [3,5..] 9 primesFrom3
    sieve (x:xs) b ~ps@(p:q:_)
      | x < b     = x : sieve xs b ps
      | otherwise =     sieve [x | x <- xs, rem x p /= 0] (q^2) (tail ps)

-- Return the prime factorization of n, using factor
primeFact :: Integer -> [Integer]
primeFact n = factor n primes

-- Factor n using factors in ps, using the algorithm described in class.
-- If factor n ps = xs, then xs is a subset of ps and product xs == n.
factor :: Integer -> [Integer] -> [Integer]
factor n (p:ps) | trace (show (n,p)) False = undefined
factor 1 ps = []
factor n (p:ps) = let (q,r) = divMod n p
                  in if r == 0 then trace ("---> " ++ show (p:ps)) p : factor q (p:ps) 
                  else factor n ps
{-
factor 120 [2,3,5]
(120,2)
[---> [2,3,5]
2(60,2)
,---> [2,3,5]
2(30,2)
,---> [2,3,5]
2(15,2)
(15,3)
,---> [3,5]
3(5,3)
(5,5)
,---> [5]
5(1,5)
]
-}

-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- insures: a^n ≡ b (mod m), 0 <= b < m
expmod :: Integer -> Integer -> Integer -> Integer
expmod a n m | trace (show (a,n,m)) False = undefined
expmod a 0 m = let ans = 1 in trace ("---> " ++ show ans) ans
expmod a n m = let (q,r) = divMod n 2
                   ans = expmod a q m
               in if r == 0 then trace ("---> " ++ show ans) ans*ans `mod` m 
               else trace ("---> " ++ show ans) ans*ans*a `mod` m
{-
expmod 5 4 9
(5,4,9)
(5,2,9)
(5,1,9)
(5,0,9)
---> 1
---> 1
---> 5
---> 7
4
-}

toBase :: Integer -> Integer -> [Integer]
toBase b n | trace (show (b,n)) False = undefined
toBase b n = reverse (toBase' b n) where
  toBase' :: Integer -> Integer -> [Integer]
  toBase' b n | trace (show (b,n)) False = undefined
  toBase' b 0 = []
  toBase' b n = let (q,r) = divMod n b
                in trace ("---> " ++ show (q,r)) r : toBase' b q
{-
toBase 4 711
(4,711)
(4,711)
(4,177)
(4,44)
(4,11)
(4,2)
(4,0)
[---> (0,2)
2,---> (2,3)
3,---> (11,0)
0,---> (44,1)
1,---> (177,3)
3]
-}

fromBase :: Integer -> [Integer] -> Integer
fromBase b (x:xs) | trace (show (b,(x:xs))) False = undefined
fromBase b xs = fromBase' b (reverse xs) where
  fromBase' :: Integer -> [Integer] -> Integer
  fromBase' b [] = let ans = 0 in trace ("---> " ++ show ans) ans
  fromBase' b (x:xs) = let ans = x + b * fromBase' b xs
                       in trace ("---> " ++ show ans) ans
{-
fromBase 4 [2,3,0,1,3]
(4,[2,3,0,1,3])
---> 0
---> 2
---> 11
---> 44
---> 177
---> 711
711
-}

-- Tracing version of extGCD (from Piazza post @68)
extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a b | trace (show (a,b)) False = undefined
extGCD a 0 = let ans = (a,1,0) in trace ("---> " ++ show ans) ans
extGCD a b = let (q,r) = divMod a b
                 (d,m,n) = extGCD b r
                 ans = (d,n,m-q*n)
             in trace ("---> " ++ show ans) ans
{-
extGCD 27 3
(27,3)
(3,0)
---> (3,1,0)
---> (3,0,1)
(3,0,1)
-}


---- Part 1: Tracing ----------------------------------------------------------

-- Create tracing versions of factor, expmod, toBase, and fromBase above
-- and cut and paste sample output here. Note that you will also need
-- to trace the helper functions toBase' and fromBase'.

---- Part 2: Boolean warm-up --------------------------------------------------

bools = [True, False]


-- We can get a list of all possible outputs of the not and && operations

not_vals = [not p | p <- bools]
and_vals = [p && q | p <- bools, q <- bools]

-- Do the same with the operators ||, <, <=, >, >=,  ==, and /=.

or_vals      = [p || q | p <- bools, q <- bools]
less_vals    = [p < q | p <- bools, q <- bools]
leq_vals     = [p <= q | p <- bools, q <- bools]
greater_vals = [p > q | p <- bools, q <- bools]
geq_vals     = [p >= q | p <- bools, q <- bools]
eq_vals      = [p == q | p <- bools, q <- bools]
neq_vals     = [p /= q | p <- bools, q <- bools]


-- The expressions p && q and q && p always give the same result, which we
-- can check in two different ways:

and1 = [p && q | p <- bools, q <- bools] == [q && p | p <- bools, q <- bools]
and2 = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Find combinations of &&, ||, and not that give the same results as
-- each of the other operators above: <, <=, >, >=, ==, and /=, and write a
-- test like either and1 or and2 to check this (it should return True).
-- I've done the first one for you in the style of and2.

less_test    = and [(p < q) == (not p && q) | p <- bools, q <- bools]

leq_test     = or [(p <= q) == (not p && q) | p <- bools, q <- bools]
greater_test = and [(p > q) == (p && not q) | p <- bools, q <- bools]
geq_test     = or [(p >= q) == (p && not q) | p <- bools, q <- bools]
eq_test      = and [(p == q) == (not p == not q) | p <- bools, q <- bools]
neq_test     = and [(p /= q) == (p ==f  not q) | p <- bools, q <- bools]
