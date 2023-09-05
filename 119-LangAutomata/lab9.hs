-- Lab 9: Derivative-based conversion from RegExp' to FSM
-- (Brzozowski Construction)

import Data.List
import Data.Array

-- Fixed alphabet, but everything below should work for any sigma!
sigma :: [Char]
sigma = "ab"

-- Normalize a list: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm xs = rad $ sort xs where
  rad :: Eq a => [a] -> [a]  -- Remove adjacent duplicates
  rad [] = []
  rad [x] = [x]
  rad (x:ys@(y:zs)) | x == y = rad ys
                    | otherwise = x : rad ys

---- Regular expressions, along with output and input
data RegExp = Empty
             | Let Char
             | Union RegExp RegExp
             | Cat RegExp RegExp
             | Star RegExp
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

-- Quick and dirty postfix regex parser, gives non-exaustive match on error
toRE :: String -> RegExp
toRE w = parse w [] where
  parse [] [r] = r
  parse ('+':xs) (r2:r1:rs) = parse xs (Union r1 r2:rs)
  parse ('.':xs) (r2:r1:rs) = parse xs (Cat r1 r2:rs)
  parse ('*':xs) (r:rs) = parse xs (Star r:rs)
  parse ('@':xs) rs = parse xs (Empty:rs)
  parse (x:xs) rs = parse xs (Let x:rs)

-- Extended regular expressions, including a name for One = Star Empty,
-- and arbitrary numbers of arguments for (associative) Union and Cat
data RegExp' = Zero | One | Let' Char |
               Union' [RegExp'] | Cat' [RegExp'] | Star' RegExp'
  deriving (Eq, Ord, Show)

-- Convert ordinary RegExps into extended RegExps
fromRE :: RegExp -> RegExp'
fromRE Empty = Zero
fromRE (Let c) = Let' c
fromRE (Union r1 r2) = Union' [fromRE r1, fromRE r2]
fromRE (Cat r1 r2) = Cat' [fromRE r1, fromRE r2]
fromRE (Star r1) = Star' (fromRE r1)

-- Convert extended RegExps into ordinary RegExps, eliminating Union' and Cat'
-- on lists of length < 2, and right-associating them on longer lists
fromRE' :: RegExp' -> RegExp
fromRE' Zero = Empty
fromRE' One = Star Empty
fromRE' (Let' c) = Let c
fromRE' (Union' []) = Empty
fromRE' (Union' [r]) = fromRE' r
fromRE' (Union' (r:rs)) = Union (fromRE' r) (fromRE' (Union' rs))
fromRE' (Cat' []) = Star Empty
fromRE' (Cat' [r]) = fromRE' r
fromRE' (Cat' (r:rs)) = Cat (fromRE' r) (fromRE' (Cat' rs))
fromRE' (Star' r) = Star (fromRE' r)

-- Basic postfix parser for RE', assuming binary + and ., including 0 and 1
toRE' :: String -> RegExp'
toRE' w = parse w [] where
  parse [] [r] = r
  parse ('0':xs) rs = parse xs (Zero:rs)
  parse ('1':xs) rs = parse xs (One:rs)
  parse ('+':xs) (r2:r1:rs) = parse xs (Union' [r1,r2]:rs)
  parse ('.':xs) (r2:r1:rs) = parse xs (Cat' [r1,r2]:rs)
  parse ('*':xs) (r:rs) = parse xs (Star' r:rs)
  parse (x:xs) rs = parse xs (Let' x:rs)


-- An extended regular expression simplifier
simp :: RegExp' -> RegExp'
simp Zero = Zero
simp One = One
simp (Let' c) = Let' c
simp (Union' rs) = union' $ flat_uni $ map simp rs
simp (Cat' rs) = union' $ flat_cat $ map simp rs
simp (Star' r) = star' $ simp r

-- Smart constructor for Union' that normalizes its arguments and
-- handles the empty and singleton cases
union' :: [RegExp'] -> RegExp'
union' rs =  case norm rs of
  []  ->  Zero
  [r] -> r
  rs  -> Union' rs

-- Smart constructor for Cat' that handles the empty and singleton cases
cat' :: [RegExp'] -> RegExp'
cat' [] = One
cat' [r] = r
cat' rs = Cat' rs

-- Smart constructor for Star' that handles the constant and Star' cases
star' :: RegExp' -> RegExp'
star' Zero = One
star' One = One
star' (Star' r) = star' r
star' r = Star' r

-- Flatten a list of arguments to Union'; assumes each argument is simplified
flat_uni :: [RegExp'] -> [RegExp']
flat_uni [] = []
flat_uni (Zero:rs) = flat_uni rs
flat_uni (Union' rs':rs) = rs' ++ flat_uni rs
flat_uni (r:rs) = r : flat_uni rs

-- Flatten a list of arguments to Cat', turning them into a list of arguments
-- to Union'; assumes each argument is simplified
flat_cat :: [RegExp'] -> [RegExp']
flat_cat rs = fc [] rs where
  -- fc (args already processed, in reverse order) (args still to be processed)
  fc :: [RegExp'] -> [RegExp'] -> [RegExp']
  fc pr [] = [cat' $ reverse pr]
  fc pr (Zero:rs) = []
  fc pr (One:rs) = fc pr rs
  fc pr (Cat' rs':rs) = fc (reverse rs' ++ pr) rs
  fc pr (Union' rs':rs) = concat $ map (fc pr . (:rs)) rs'
  fc pr (r:rs) = fc (r:pr) rs


-- Finite state machines, now indexed by the type of their states
-- M = (states, start, finals, transitions)  
type FSM a = ([a], a, [a], a -> Char -> a)


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

-- Unary set closure (where "set" = normalized list)
-- uclosure xs g == smallest set containing xs and closed under g
uclosure :: Ord a => [a] -> (a -> [a]) -> [a]
uclosure xs g = sort $ stable $ iterate close (xs, []) where
  stable ((fr,xs):rest) = if null fr then xs else stable rest
  close (fr, xs) = (fr', xs') where
    xs' = fr ++ xs
    fr' = norm $ filter (`notElem` xs') $ concatMap g fr


---------------- Lab 9 begins here ----------------

-- Bypassable for extended REs, computed directly by recursion.
byp :: RegExp' -> Bool
byp (Zero)      = False
byp (One)       = True
byp (Let' a)    = False
byp (Union' rs) = and [byp x | x <- rs]
byp (Cat' rs)   = or [byp x | x <- rs]
byp (Star' r)   = True

-- Regular-expression derivatives (aka left quotients) on extended REs,
-- computed directly by recursion.
deriv :: Char -> RegExp' -> RegExp'
deriv a (Zero)    = Zero
deriv a (One)     = Zero
deriv a (Let' b)  | a == b    = One
                  | otherwise = Zero
deriv a (Union' rs) = Union' (map (\r -> deriv a r) rs)
deriv a (Cat' rs) = Union' (derivh rs) where
  derivh :: [RegExp'] -> [RegExp']
  derivh [] = []
  derivh (r:rs)   | byp r     = Cat' ((deriv a r):rs) : (derivh rs)
                  | otherwise = Cat' ((deriv a r):rs) : []
deriv a (Star' r) = Cat' ([deriv a r] ++ [Star' r])

-- Convert an RegExp' to an FSM using the derivative (Brzozowski) construction.
-- States are SIMPLIFIED extended REs.  Note: to construct all the states,
-- you will have to use another closure process.
conv :: RegExp' -> FSM RegExp'
conv r = (qs,s,fs,d) where
  qs    = uclosure [simp r] (\r -> [simp (deriv a r) | a <- sigma])
  s     = simp r
  fs    = [q | q <- qs, byp q]
  d q a = simp (deriv a q)


-- Test, and show your tests! You may copy code from previous labs to help.

-- *Main> r = toRE' "abab.+*.b.ab+."
-- *Main> Compact (fromRE' r)
-- a(b+ab)*b(a+b)
-- *Main> (qs,s,fs,d) = conv r
-- *Main> qs
-- [Zero,Union' [One,Let' 'a',Let' 'b',Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'a'],
-- Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),
-- Let' 'b',Let' 'b']],Union' [One,Cat' [Let' 'b',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 
-- 'b']]),Let' 'b',Let' 'a'],Cat' [Let' 'b',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'b']],
-- Union' [Let' 'a',Let' 'b',Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'a'],
-- Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'b']],
-- Union' [Cat' [Let' 'a',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'a'],
-- Cat' [Let' 'a',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 
-- 'b',Let' 'b']],Union' [Cat' [Let' 'b',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'a'],
-- Cat' [Let' 'b',Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'b']],
-- Union' [Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'a'],
-- Cat' [Star' (Union' [Let' 'b',Cat' [Let' 'a',Let' 'b']]),Let' 'b',Let' 'b']]]
-- *Main> length qs                 
-- 7
-- *Main> map (Compact . fromRE') qs
-- [@,@*+a+b+(b+ab)*ba+(b+ab)*bb,@*+b(b+ab)*ba+b(b+ab)*bb,a+b+(b+ab)*ba+(b+ab)*bb,
-- a(b+ab)*ba+a(b+ab)*bb,b(b+ab)*ba+b(b+ab)*bb,(b+ab)*ba+(b+ab)*bb]
-- *Main> (Compact . fromRE') s     
-- a(b+ab)*ba+a(b+ab)*bb
-- *Main> map (Compact . fromRE') fs
-- [@*+b(b+ab)*ba+b(b+ab)*bb,a(b+ab)*ba+a(b+ab)*bb,b(b+ab)*ba+b(b+ab)*bb,(b+ab)*ba+(b+ab)*bb]

-- *Main> r = toRE' "b*a.ab+*."
-- *Main> Compact (fromRE' r)
-- b*a(a+b)*
-- *Main> (qs,s,fs,d) = r
-- *Main> map (Compact . fromRE') qs
-- [b*a(a+b)*,(a+b)*]
-- *Main> (Compact . fromRE') s     
-- b*a(a+b)*
-- *Main> map (Compact . fromRE') fs
-- [b*a(a+b)*,(a+b)*]
-- *Main> deriv 'a' r
-- Union' [Cat' [Union' [Cat' [Cat' [Zero,Star' (Let' 'b')],Let' 'a'],Cat' [One]],Star' (Union' [Let' 'a',Let' 'b'])],
-- Cat' [Cat' [Union' [One,Zero],Star' (Union' [Let' 'a',Let' 'b'])]]]
-- *Main> (Compact . fromRE') it    
-- (@b*a+@*)(a+b)*+(@*+@)(a+b)*
-- *Main> deriv 'b' r
-- Union' [Cat' [Union' [Cat' [Cat' [One,Star' (Let' 'b')],Let' 'a'],Cat' [Zero]],Star' (Union' [Let' 'a',Let' 'b'])],
-- Cat' [Cat' [Union' [Zero,One],Star' (Union' [Let' 'a',Let' 'b'])]]]
-- *Main> (Compact . fromRE') it
-- (@*b*a+@)(a+b)*+(@+@*)(a+b)*

-- From Lab 7:
-- Solve a system of proper linear equations
-- You can assume that the system is correctly formed and proper
solve :: [[RegExp']] -> [RegExp'] -> [RegExp']

solve [] [] = []
solve ((l11:l1J) : rows) (l1':lI') = simp x1 : xI where
  -- l11 is the corner element, and l1J = [l12,...,l1n] is the rest of 1st row
  -- rows are the rest of the rows [[l21,...,l2n], ..., [ln1,...,lnn]]
  -- l1' is the first constant
  -- lI' are the rest of the contants [l2',...,ln']
  
  -- first column [l21, ..., ln1]
  lI1 = map head rows

  -- sub-matrix [[l22,...,l2n], ..., [ln2,...,lnn]]
  lIJ = map tail rows

  -- [[l22_bar,...,l2n_bar], ..., [ln2_bar,...,lnn_bar]] computed via (6)
  lIJ_bar = zipWith sixes lI1 lIJ            -- loops for i = 2 .. n
  sixes li1 liJ = zipWith (six li1) l1J liJ  -- loops for j = 2 .. n
  six li1 l1j lij = Union' [(Cat' [li1, Star' l11, l1j]), lij]

  -- [l2'_bar,..., ln'_bar] computed via (7)
  lI'_bar = zipWith seven lI1 lI'
  seven li1 li' = Union' [(Cat' [li1, Star' l11, l1']), li']
    
  -- recursively solve the system of size n-1
  xI = solve lIJ_bar lI'_bar

  -- compute x1 from xI via (5)
  x1 = Cat' [Star' l11, Union' (zipWith (\x y -> Cat' [x,y]) l1J xI ++ [l1'])]

-- Generate a regular SPLE from an FSM via formulas in Theorem 6.5
toSPLE :: FSM Int -> ([[RegExp']], [RegExp'])
toSPLE (qs,s,fs,d) = (lIJ, lI') where
  
  -- Construct matrix of coefficients (coef i j = Lij)
  lIJ = [[simp (coef i j) | j <- qs] | i <- qs]
  coef i j = Union' [Cat' [Let' a, Star' Zero] | a <- sigma, d i a == j]

  -- Construct constants
  lI' = [if elem q fs then One else Zero | q <- qs]

-- Convert an FSM to a RegExp'
convfsm :: FSM Int -> RegExp'
convfsm m@(_,s,_,_) = simp $ solution !! s where
  (matrix, consts) = toSPLE m
  solution = solve matrix consts

-- From Lab 5:
oddbs :: FSM Int
oddbs = ([0,1], 0, [1], d) where
  d q 'a' = q        -- stay in the same state
  d q 'b' = 1 - q    -- switch states

avoid_aab :: FSM Int
avoid_aab = ([0,1,2,3], 0, [0,1,2], d) where
  d 0 'a' = 1;  d 1 'b' = 0
  d 0 'b' = 0;  d 1 'a' = 2
  d 2 'a' = 2;  d 2 'b' = 3
  d _ _ = 3

end_ab :: FSM Int
end_ab = ([0,1,2], 0, [2], d) where
  d 0 'a' = 1;  d 0 'b' = 0
  d 1 'a' = 1;  d 1 'b' = 2
  d 2 'a' = 1;  d 2 'b' = 0

-- *Main> r = convfsm oddbs 
-- *Main> Compact (fromRE' r)
-- a*b(a+ba*b)*
-- *Main> (qs,s,fs,d) = conv r
-- *Main> map (Compact . fromRE') qs
-- [a*b(a+ba*b)*,(a+ba*b)*]
-- *Main> (Compact . fromRE') s     
-- a*b(a+ba*b)*
-- *Main> map (Compact . fromRE') fs
-- [a*b(a+ba*b)*,(a+ba*b)*]

-- *Main> r = convfsm avoid_aab
-- *Main> Compact (fromRE' r)
-- b*a(bb*a)*+b*a(bb*a)*aa*+b*a(bb*a)*bb*+b*
-- *Main> (qs,s,fs,d) = conv r
-- *Main> map (Compact . fromRE') qs
-- [@,b*a(bb*a)*+b*a(bb*a)*aa*+b*a(bb*a)*bb*+b*,(bb*a)*aa*+(bb*a)*bb*+(bb*a)*,a*]
-- *Main> (Compact . fromRE') s     
-- b*a(bb*a)*+b*a(bb*a)*aa*+b*a(bb*a)*bb*+b*
-- *Main> map (Compact . fromRE') fs
-- [b*a(bb*a)*+b*a(bb*a)*aa*+b*a(bb*a)*bb*+b*,(bb*a)*aa*+(bb*a)*bb*+(bb*a)*,a*]

-- *Main> r = convfsm end_ab
-- *Main> Compact (fromRE' r)
-- b*aa*b(aa*b+bb*aa*b)*
-- *Main> (qs,s,fs,d) = conv r
-- *Main> map (Compact . fromRE') qs
-- [a*b(aa*b+bb*aa*b)*,b*aa*b(aa*b+bb*aa*b)*,(aa*b+bb*aa*b)*]       
-- *Main> (Compact . fromRE') s     
-- b*aa*b(aa*b+bb*aa*b)*
-- *Main> map (Compact . fromRE') fs
-- [a*b(aa*b+bb*aa*b)*,b*aa*b(aa*b+bb*aa*b)*,(aa*b+bb*aa*b)*]