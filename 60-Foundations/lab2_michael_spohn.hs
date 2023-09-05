-- CSci 60 Lab 2
-- Michael Spohn 110360841

---------------- Part 1 ----------------
--
-- Answer the following questions based on the material in Chapters 2 and 3
-- of the LYaHfGG tutorial.

-- Define a function, ranges, using if-then-else, that returns 3 on
-- numbers from 0 to 10 5 on numbers from 11 to 20, 1 on numbers from
-- 21 to 30, and 0 on everything else

ranges :: Int -> Int
ranges n = if n >= 0 && n <= 10
              then 3
           else if n > 10 && n <= 20
              then 5
           else if n > 20 && n <= 30
              then 1
           else 0

-- Define a function, palindrome, that takes a list a returns another list
-- that is the same forwards and backwards and starts with the given list.
-- For example, palidrome [5, 2, 7] = [5, 2, 7, 7, 2, 5]

palindrome :: [Int] -> [Int]
palindrome xs = xs ++ reverse xs


-- Define a function, middle, that takes a list and returns the "middle" of
-- the list, that is, all of its elements except the first and last.
-- For example, middle [1, 8, 3, 6, 2] = [8, 3, 6]

middle :: [Int] -> [Int]
middle xs = (init(tail xs))


-- Write expressions whose values are the sums of
-- (1) all even numbers between 100 and 200 (including 100 and 200)
-- (2) every third number from 100 to 199

sum1 = sum [ n | n <- [100,102 .. 200]]
sum2 = sum [ n | n <- [100,103 .. 199]]


-- Write list comprehensions to generate the following lists:
-- (1) all numbers between 100 and 200 not divisible by 3, 5, or 7
-- (2) the squares of the numbers from 30 to 50 ([30*30, 31*31, ..., 50*50])
-- (3) all possible products of three (not necessarily different elements
--     from the list [11,16,21,27]

list1 = filter (\x -> mod x 3>0 && mod x 5>0 && mod x 7>0) [100 .. 200]
list2 = map (^2) [30 .. 50]
list3 = [ x*y*z | x <- [11,16,21,27], y <- [11,16,21,27], z <- [11,16,21,27]]


-- How many numbers from the list [150, 151, 152, ...] must be added before the
-- sum becomes larger than 1,000,000?  Write an expression whose value is the
-- answer to this question.

ans = [ takeWhile (<=1000000)x | x <- [scanl (+)0[150 ..]]]


---------------- Part 1.5 ----------------
--
-- Work through Chapter 4 of LYaHfGG



---------------- Part 2 ----------------
--
-- Replace the instances of "undefined" in the following definition
-- so that the following function computes the extended GCD of a and b.
-- Inputs a,b >= 0; Output: (d,m,n) such that d = gcd a b and
-- d = m * a + n * b.

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a,1,0)
extGCD a b = let (q,r) = divMod a b
                 (d,m,n) = extGCD b r
             in (d,n,m-q*n)