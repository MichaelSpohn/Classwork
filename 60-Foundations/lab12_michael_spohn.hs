-- CSci 60 Lab 
-- Michael Spohn
-- 110360841


-- Sets and relations. Neither sets nor relations should contain duplicates.
type Set = [Int]
type Rel = [(Int,Int)]

-- Two given sets a and b. Use these as the "universes" for testing,
-- but you should write your code so that these sets can be replaced
-- by any two finite sets.
a,b :: Set
a = [1,2,3,4]
b = [5,6,7,8]


---------------- Part 1 ----------------

-- Write tests to check whether a relation between a and b is
-- total, unique, onto (aka surjective), and injective (aka one-to-one).
-- You should assume that rs is a subset of a x b (no need to check)

total :: Rel -> Bool
total rs = and [or [elem (x,y) rs | y <- b] | x <- a]

uniq :: Rel -> Bool
uniq rs = and [(elem (x,y) rs && elem (x,z) rs) <= (y == z) | x <- a, y <- b, z <- b]

onto :: Rel -> Bool
onto rs = and [or [elem (x,y) rs | x <- a] | y <- b]

inj :: Rel -> Bool -- One-to-One
inj rs = and [(elem (x,y) rs && elem (z,y) rs) <= (x == z) | x <- a, y <- b, z <- a]


-- For each of the 16 possible combinations of yes/no on total, unique,
-- onto, and one-to-one, find a minimal relation on u (i.e., one with the
-- fewest number of pairs) that has exactly that combination of properties.
-- Many of these combinations are impossible; leave these undefined.
-- Unlike the last lab, I'm not telling you which ones are impossible,
-- or even how many of them are impossible.  I've included the tests that
-- confirm the properties. Note: please FIND the relation first, THEN test!!

-- total, uniq, onto, inj
tuoi :: Rel
tuoi = [(1,5), (2,6), (3,7), (4,8)]
tuoi_test = total tuoi && uniq tuoi && onto tuoi && inj tuoi -- True

-- total, uniq, onto, not inj
tuoi' :: Rel
tuoi' = undefined
tuoi'_test = total tuoi' && uniq tuoi' && onto tuoi' && not (inj tuoi') -- Impossible
{- In order to remove inj, we'd have to either break onto or total in order to
   make inj not work. Soley breaking inj is not possible since all values in both
   a and b can't be included for total and onto without breaking unique. -}

-- total, uniq, not onto, inj
tuo'i :: Rel
tuo'i = undefined
tuo'i_test = total tuo'i && uniq tuo'i && not (onto tuo'i) && inj tuo'i -- Impossible
{- By removing onto, it's not possible to complete the requirement for total to
   work, since the fourth value from a can't go anywhere. Unless one of the other
   three were okay to break, soley breaaking onto wouldn't work. -}

-- total, uniq, not onto, not inj
tuo'i' :: Rel
tuo'i' = [(1,5), (2,5), (3,5), (4,5)]
tuo'i'_test = total tuo'i' && uniq tuo'i' && not (onto tuo'i') && not (inj tuo'i') -- True

-- total, not uniq, onto, inj
tu'oi :: Rel
tu'oi = undefined
tu'oi_test = total tu'oi && not (uniq tu'oi) && onto tu'oi && inj tu'oi -- Impossible
{- Similarly with tuoi', for total and onto to work, every value is needed to be
   used once, but to break unique and keep total and onto, inj would have to break
   as well. -}

-- total, not uniq, onto, not inj
tu'oi' :: Rel
tu'oi' = [(1,5), (2,5), (2,6), (3,7), (4,8)]
tu'oi'_test = total tu'oi' && not (uniq tu'oi') && onto tu'oi' && not (inj tu'oi') -- True

-- total, not uniq, not onto, inj
tu'o'i :: Rel
tu'o'i = undefined
tu'o'i_test = total tu'o'i && not (uniq tu'o'i) && not (onto tu'o'i) && inj tu'o'i -- Impossible
{- B is not large enough so that total can be accomplished with total and not break
   inj. Inj will break when trying to do not unique and total with the amount of
   values in B. -}

-- total, not uniq, not onto, not inj
tu'o'i' :: Rel
tu'o'i' = [(1,5), (1,6), (2,5), (3,6), (4,6)]
tu'o'i'_test = total tu'o'i' && not (uniq tu'o'i') && not (onto tu'o'i') && not (inj tu'o'i') -- True

-- not total, uniq, onto, inj
t'uoi :: Rel
t'uoi = undefined
t'uoi_test = not (total t'uoi) && uniq t'uoi && onto t'uoi && inj t'uoi -- Impossible
{- Just like with tuo'i, the fourth value from B won't have an A in order to avoid
   total and accomplish onto without breaking unique or inj. If there was another
   value in A, then it would've worked. -}

-- not total, uniq, onto, not inj
t'uoi' :: Rel
t'uoi' = undefined
t'uoi'_test = not (total t'uoi') && uniq t'uoi' && onto t'uoi' && not (inj t'uoi') -- Impossible
{- This combination can't work due to there being the same amount of values in A and
   B. In order to break total and inj but still satisfy unique and onto, more values
   are needed in A so that unique and onto can be satisfied without satisfying total. -}

-- not total, uniq, not onto, inj
t'uo'i :: Rel
t'uo'i = []
t'uo'i_test = not (total t'uo'i) && uniq t'uo'i && not (onto t'uo'i) && inj t'uo'i -- True

-- not total, uniq, not onto, not inj
t'uo'i' :: Rel
t'uo'i' = [(1,5), (2,5)]
t'uo'i'_test = not (total t'uo'i') && uniq t'uo'i' && not (onto t'uo'i') && not (inj t'uo'i') -- True

-- not total, not uniq, onto, inj
t'u'oi :: Rel
t'u'oi = [(1,5), (1,6), (1,7), (1,8)]
t'u'oi_test = not (total t'u'oi) && not (uniq t'u'oi) && onto t'u'oi && inj t'u'oi -- True

-- not total, not uniq, onto, not inj
t'u'oi' :: Rel
t'u'oi' = [(1,5), (1,6), (2,6), (2,7), (2,8)]
t'u'oi'_test = not (total t'u'oi') && not (uniq t'u'oi') && onto t'u'oi' && not (inj t'u'oi') -- True

-- not total, not uniq, not onto, inj
t'u'o'i :: Rel
t'u'o'i = [(1,5), (1,6)]
t'u'o'i_test = not (total t'u'o'i) && not (uniq t'u'o'i) && not (onto t'u'o'i) && inj t'u'o'i -- True

-- not total, not uniq, not onto, not inj
t'u'o'i' :: Rel
t'u'o'i' = [(1,5), (2,5), (2,6)]
t'u'o'i'_test = not (total t'u'o'i') && not (uniq t'u'o'i') && not (onto t'u'o'i') && not (inj t'u'o'i') -- True



---------------- Part 2 ----------------

-- Given a relation rs between a and b, return the converse relation rs^-1
-- between b and a
converse :: Rel -> Rel
converse rs = undefined

-- Given a relation rs1 on a (i.e., between a and a) and a relation rs2
-- between a and b, return the composite relation rs2 o rs1 between a and b
compose :: Rel -> Rel -> Rel
compose rs1 rs2 = undefined

-- Given a relation rs between a and b, and a subset xs of a,
-- return the direct image rs(xs) as a subset of b
direct :: Rel -> Set -> Set
direct rs xs = undefined

-- Given a relation rs between A and B, and a subset ys of BA,
-- return the inverse image rs^-1(ys) as a subset of a
inverse :: Rel -> Set -> Set
inverse rs ys = undefined
