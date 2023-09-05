-- CSci 60 Lab 1
-- Michael Spohn 110360841

---------------- Part 1 ----------------

-- Work through Chapters 1 - 3 of Learn You a Haskell for Great Good!
-- Type in the examples and make sure you understand the results.
-- Ask questions about anything you don't understand! This is your
-- chance to get off to a good start understanding Haskell.


---------------- Part 2 ----------------

-- The Haskell Prelude has a lot of useful built-in functions related
-- to numbers and lists.  In Part 2 of this lab, you will catalog many
-- of these functions.

data Color = Red | Orange | Yellow | Green | Blue | Violet
     deriving (Show, Eq, Ord, Enum)

-- For each of the Prelude functions listed below, give its type,
-- describe in your own words what the function does, answer any
-- questions specified, and give several examples of its use.  Does
-- the function apply at all to the "Color" type defined above?


-- succ, pred
-- succ:	Type:  succ :: Enum a => a -> a
--			Info:  Goes forward to the next value (succeeds) based on what was input.
--			Color: Can be used to go to the next color that is listed.
--			Exs:   succ 9 => 10
--				   succ Red => Orange
--				   succ 'a' => 'b'
-- pred:	Type:  pred :: Enum a => a -> a
--			Info:  Goes backwards to the inputted value's predecessor
--			Color: Can be used to go to the previous color that is listed.
--			Exs:   pred 10 => 9
--				   pred Blue => Green
--				   pred 'b' => 'a'

-- toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo
-- toEnum:	Type:  toEnum :: Enum a => Int -> a
--			Info:  Outputs whatever value is listed at the inputted position.
--			Color: Can output a color based on the position number that is inputted
--				   since the colors are enumerated.
--			Exs:   toEnum 3 :: Color => Green
--				   toEnum 0 :: Bool => False
--				   toEnum 5 :: Int => 5
-- fromEnum:Type:  fromEnum :: Enum a => a -> Int
--			Info:  The opposite of toEnum; the output is the position of the
--				   value's list.
--			Color: Can output the position of a inputted color value.
--			Exs:   fromEnum Green => 3
--				   fromEnum True => 1
--				   fromEnum 2 => 2
-- enumFrom:Type:  enumFrom :: Enum a => a -> [a]
--			Info:  Shows the remainder of a list starting at the value that was
--				   inputted.
--			Color: Can output the remainder of the Color list based on what color
--				   the user inputted with this function.
--			Exs:   enumFrom Green => [Green,Blue,Violet]
--				   enumFrom False => [False,True]
--				   enumFrom Red => [Red,Orange,Yellow,Green,Blue,Violet]
-- enumFromThen: Type:  enumFromThen :: Enum a => a -> a -> [a]
--				 Info:  Outputs values of a list that are distanced apart based on
--						what was inputted.
--				 Color: Works with any other list and will output a list with
--						some colors missing as they were taken out for the list's
--						values to have distance.
--				 Exs:   enumFromThen Red Yellow => [Red,Yellow,Blue]
--				   		take 3 (enumFromThen 5 10) => [5,10,15]
--				   		take 10 (enumFromThen 'a' 'c') => "acegikmoqs"
-- enumFromTo: Type:  enumFromTo :: Enum a => a -> a -> [a]
--			   Info:  Outputs an inputted range of a list.
--			   Color: Can output a start and end with the list of colors.
--			   Exs:   enumFromTo Red Yello => [Red,Orange,Yellow]
--				   	  enumFromTo 1 10 => [1,2,3,4,5,6,7,8,9,10]
--				      enumFromTo 'a' 'f' => "abcdef"
-- enumFromThenTo: Type:  enumFromThenTo :: Enum a => a -> a -> a -> [a]
--				   Info:  Outputs a list based on the inputs of a starting point,
--						  distance between values, and ending point (all taken from
--						  an already existing list.)
--				   Color: Can choose a starting color from the list, a distance
--						  between the other colors, and a stopping point for the
--						  enumerated list.
--				   Exs:   enumFromThenTo Orange Green Blue => [Orange,Green]
--				   		  enumFromThenTo 1 10 20 => [1,10,19]
--				   		  enumFromThenTo 'a' 'c' 'm' => "acegikm" 
-- As one of your examples, try  (toEnum 3) :: Color

-- ==, /=
-- ==:		Type:  (==) :: Eq a => a -> a -> Bool
--			Info:  Used to compare two values and see whether they are the same
--			Color: Can be used to compare two colors and see if they're the same
--			Exs:   Red == Red => True
--				   2 == 4 => False
--				   (2 + 2) == 4 => True
-- /=:		Type:  (/=) :: Eq a => a -> a -> Bool
--			Info:  Used to compare two values and see whether they are different
--			Color: Can be used to compare two colors and see if they're different
--			Exs:   Red /= Orange => True
--				   Red /= Red => False
--				   (1000 * 5) /= 0 => True

-- quot, div            (Q: what is the difference? Hint: negative numbers)
--						 A: Both quot and div round down to an integer value,
--							however when it comes to negative values, quot will
--							round up to a lower negative while div will round down
--							to a higher negative. For example, (-5) divided by 2...
--							with quot it will result in -2, but with div it gives -3.
--							Therefore, when dealing with negatives, quot will round
--							in the positive direction while div will round in the
--							negative direction.
-- quot:	Type:  quot :: Integral a => a -> a -> a
--			Info:  Divides two integers and rounds down in the quotient.
--			Color: Doesn't work with the Color function.
--			Exs:   quot 10 2 => 5
--				   5 `quot` 2 => 2
--				   (-5) `quot` 2 => -2
-- div:		Type:  div :: Integral a => a -> a -> a
--			Info:  Divides two integers and rounds down in the quotient, except
--				   with negatives.
--			Color: Doesn't work with the Color function.
--			Exs:   div 10 2 => 5
--				   5 `div` 2 => 2
--				   (-5) `div` 2 => -3

-- rem, mod             (Q: what is the difference? Hint: negative numbers)
--						 A: Both rem and mod output the remainder of division with
--							integers, however the difference between them is with
--							how negatives are used. With rem, if the first input is
--							the only negative value, then the remainder will be
--							negative. With mod, if the second input is the only
--							negative value, then the remainder will be negative.
--							Otherwise, there is no remainder or it is positive.
-- rem:		Type:  rem :: Integral a => a -> a -> a
--			Info:  Outputs the remainder from division with integers (quot, div)
--			Color: Doesn't work with the Color function.
--			Exs:   rem 5 2 => 1
--				   (-5) `rem` 2 => -1
--				   10 `rem` 2 => 0
-- mod:		Type:  mod :: Integral a => a -> a -> a
--			Info:  Outputs the remainder from division with integrals similarly to
--				   rem. The different is with how negatives are handled.
--			Color: Doesn't work with the Color function.
--			Exs:   mod 10 2 => 0
--				   mod 5 2 => 1
--				   5 `mod` (-2) => -1

-- quotRem, divMod
-- quotRem:	Type:  quotRem :: Integral a => a -> a -> (a, a)
--			Info:  Uses the properties of quot and rem to divide 2 integers and
--				   outputs the quotient and remainder.
--			Color: Doesn't work with the Color function.
--			Exs:   quotRem 10 2 => (5,0)
--				   quotRem (-15) 2 => (-7,1)
--				   quotRem (-22) 3 => (-7,1)
-- divMod:	Type:  divMod :: Integral a => a -> a -> (a, a)
--			Info:  Uses the properties of div and mod to divide 2 integers and
--				   outputs the quotient and remainder/modular.
--			Color: Doesn't work with the Color function.
--			Exs:   divMod 10 2 => (5,0)
--				   divMod (-17) 2 => (-9,1)
--				   divMod 50 (-3) => (-17,-1)

-- &&, ||
-- &&:		Type:  (&&) :: Bool -> Bool -> Bool
--			Info:  The boolean sign for "and" and will result in True or False.
--			Color: Doesn't work with the Color function.
--			Exs:   True && True => True
--				   False && False => False
--				   True && False => False
-- ||:		Type:  (||) :: Bool -> Bool -> Bool
--			Info:  The boolean sign for "or" and will result in True or False.
--			Color: Doesn't work with the Color function.
--			Exs:   True || True => True
--				   True || False => True
--				   False || False => False

-- ++:		Type:  (++) :: [a] -> [a] -> [a]
--			Info:  Concatenates (combines) multiple lists or other values together.
--			Color: Can create a list of selected colors.
--			Exs:   [Red] ++ [Orange] => [Red,Orange]
--				   "Hello " ++ "world!" => "Hello world!"
--				   [1,2,3,4,5] ++ [-1,0,1,2,3] => [1,2,3,4,5,-1,0,1,2,3]

-- compare: Type:  compare :: Ord a => a -> a -> Ordering
--			Info:  Compares two values and outputs LT (less than), EQ (equal to), or
--				   GT (greater than).
--			Color: Can compare two colors from the Color function using the positions
--				   each color is in.
--			Exs:   compare Red Yellow => LT
--				   compare 2 2 => EQ
--				   compare 10 2 => GT

-- <, >
-- <:		Type:  (<) :: Ord a => a -> a -> Bool
--			Info:  Compares two values by outputting whether or not the first value
--				   is less than the second value (with True or False).
--			Color: Can compare two colors and state if the first color is or isn't
--				   less than the other.
--			Exs:   Red < Yellow => True
--				   5 < 7 => True
--				   5 < (7-3) => False
-- >:		Type:  (>) :: Ord a => a -> a -> Bool
--			Info:  Compares two values by outputting whether or not the first value
--				   is greater than the second value (with True or False).
--			Color: Can compare two colors and state if the first color is or isn't
--				   greater than the other.
--			Exs:   Yellow > Red => True
--				   5 > 20 => False
--				   1 > 1 => False

-- max, min
-- max:		Type:  max :: Ord a => a -> a -> a
--			Info:  Outputs the largest of two inputted values.
--			Color: Can be used to output the color with the higher position over
--				   another.
--			Exs:   max Red Yellow => Yellow
--				   max 10 100 => 100
--				   max 'a' 'd' => 'd'
-- min:		Type:  min :: Ord a => a -> a -> a
--			Info:  Outputs the smallest of two inputted values.
--			Color: Can be used to output the color with the lower position over
--				   another.
--			Exs:   min Red Green => Red
--				   min 10 100 => 10
--				   min 'L' 'l'

-- ^: 		Type:  (^) :: (Integral b, Num a) => a -> b -> a
--			Info:  Raises a number to a power (that has to be an integer).
--			Color: Doesn't work with the Color function.
--			Exs:   3^2 => 9
--				   (1+4)^2 => 25
--				   25^2 => 625

-- all, any
-- all:		Type:  all :: Foldable t => (a -> Bool) -> t a -> Bool
--			Info:  Outputs True or False based on if the whole inputted list fulfills
--				   the stated condition.
--			Color: Can be used to see if a list of colors pass a related condition.
--			Exs:   all (==Red) [Red,Green] => False
--				   all (>'a') ['b','c'] => True
--				   all (<0) [-1,-2,-3,-4] => True
-- any:		Type:  any :: Foldable t => (a -> Bool) -> t a -> Bool
--			Info:  Outputs True or False based on if one value in an inputted list
--				   fulfills the stated condition.
--			Color: Can be used to see if at least one color in a list of colors
--				   can pass a related condition.
--			Exs:   any (>Red) [Red,Orange,Yellow] => True
--				   any (==0) [0,1] => True
--				   any (<0) [0,1,2,3,4] => False

-- break:	Type:  break :: (a -> Bool) -> [a] -> ([a], [a])
--			Info:  Breaks apart a list into a tuple of two lists.
--			Color: Can break apart a list of colors into two lists of colors.
--			Exs:   break (Yellow==) [Red,Yellow] => ([Red],[Yellow])
--				   break (2==) [0,1,2,3,4,5] => ([0,1],[2,3,4,5])
--				   break (>2) [0,1,3,4] => ([0,1],[3,4])

-- concat:	Type:  concat :: Foldable t => t [a] -> [a]
--			Info:  Does the opposite of break and concantenates two inputted lists,
--				   outputting one list.
--			Color: Can concantenate two lists of colors into one list.
--			Exs:   concat [[Red,Yellow],[Green,Blue]] => [Red,Yellow,Green,Blue]
--				   concat [[1,2],[3,4]] => [1,2,3,4]
--				   concat ["Hello ", "there."] => "Hello there."

-- const:	Type:  const :: a -> b -> a
--			Info:  Takes two inputs and outputs the first input (the constant)
--			Color: Works with Color values and can output the color inputted first.
--			Exs:   const Red Yellow => Red
--				   const 5 2 => 5
--				   const 'a' 'b' => 'a'

-- cycle:	Type:  cycle :: [a] -> [a]
--			Info:  Repeats a list an infinite amount of times unless limited.
--			Color: Can repeat a list of colors a set amount of times.
--			Exs:   take 5 (cycle [Red,Blye]) => [Red,Blue,Red,Blue,Red]
--				   take 4 (cycle [(2*2)]) => [4,4,4,4]
--				   take 11 (cycle "Hello ") => "Hello Hello"

-- drop, take
-- drop:	Type:  drop :: Int -> [a] -> [a]
--			Info:  Outputs a select amount of values from a list by dropping
--				   values from the beginning and keeping the rest.
--			Color: This function can drop some amount of values from the
--				   beginning of a list of colors and output the rest.
--			Exs:   drop 2 [1,2,3,4] => [3,4]
--				   drop 2 [Red,Red,Green,Green] => [Green,Green]
--				   drop 6 "Hello Hello" => "Hello"
-- take:	Type:  take :: Int -> [a] -> [a]
--			Info:  Outputs a select amount of values from a list (starting at
--				   the beginning).
--			Color: This function can take some amount of values from the
--				   beginning of a list of colors.
--			Exs:   take 2 [Red,Green,Blue,Green] => [Red,Green]
--				   take 2 [1,2,3,4] => [1,2]
--				   take 3 "abcd" => "abc"

-- dropWhile, takeWhile
-- dropWhile: Type:  dropWhile :: (a -> Bool) -> [a] -> [a]
--			  Info:  Works like drop but uses a condition to determine when
--					 to stop dropping values from the inputted list.
--			  Color: Just like with drop, a list of colors can be modified
--					 through dropping values.
--			  Exs:   dropWhile (==Red) [Red,Red,Red,Green] => [Green]
--				     dropWhile (5>) [1,2,3,4,5,6,7] => [5,6,7]
--				     dropWhile (<3) [1,2,3] => [3]
-- takeWhile: Type:  takeWhile :: (a -> Bool) -> [a] -> [a]
--			  Info:  Works like take but uses a condition to determine when
--					 to stop accepting values to output into a modified list.
--			  Color: Just like with take, a list of colors can be modified
--					 by accepting values from the beginning.
--			  Exs:   takeWhile (==Blue) [Blue,Blue,Red] => [Blue,Blue]
--				     takeWhile (<0) [-3,-2,-1,0,1,2,3] => [-3,-2,-1]
--				     takeWhile (5>) [4,5,6] => [4]

-- elem:	Type:  elem :: (Foldable t, Eq a) => a -> t a -> Bool
--			Info:  Outputs whether or not a value is apart of a list using
--				   True or False.
--			Color: Can be used to see if a color is apart of a list.
--			Exs:   elem Red [Red,Green] => True
--				   elem 4 [1,2,3,4] => True
--				   0 `elem` [1,2,3] => Fale

-- even:	Type:  even :: Integral a => a -> Bool
--			Info:  Outputs whether or not a number is even using True or
--				   False.
--			Color: Since colors aren't numbers, this function can't be used.
--			Exs:   even 5 => False
--				   even 0 => True
--				   even (-2) => True

-- filter:	Type:  filter :: (a -> Bool) -> [a] -> [a]
--			Info:  Works similarly to take as this function will take values
--				   from a list if they fulfill a condition, however the
--				   start can be anywhere and the list is overall filtered.
--			Color: Can filter out a list of colors just like drop and take.
--			Exs:   filter (/=Red) [Red,Green,Red,Blue] => [Green,Blue]
--				   filter even [1,2,3,4,5,6] => [2,4,6]
--				   filter (>0) [0,1,-1,0,-1,2,0,1] => [1,2,1]

-- fst:		Type:  fst :: (a, b) -> a
--			Info:  Outputs the first value of an inputted pair.
--			Color: Colors can be used in pairs, therefore if the first value
--				   is a color, it can still be outputted.
--			Exs:   fst (Red,Green) => Red
--				   fst ("Yes", "No") => "Yes"
--				   fst (True, False) => True

-- gcd:		Type:  gcd :: Integral a => a -> a -> a
--			Info:  Outputs the largest number that can divide into both inputted
--				   numbers.
--			Color: Since this is a function that handles numbers, the Color
--				   function can't be used.
--			Exs:   gcd 20 10 => 10
--				   gcd 15 2 => 1
--				   gcd 10 10 => 10

-- head:	Type:  head :: [a] -> a
--			Info:  Outputs the first value of an inputted list.
--			Color: Can output a color if it's the first value of a list.
--			Exs:   head [Red,Green,Blue] => Red
--				   head [0,1,2,3] => 0
--				   head ["Hello","world"] => "Hello"

-- id:		Type:  id :: a -> a
--			Info:  Returns the input.
--			Color: Returns the inputted color.
--			Exs:   id Red => Red
--				   id 2 => 2
--				   id (4-2) => 2

-- init:	Type:  init :: [a] -> [a]
--			Info:  Outputs an inputted list that excludes the last value.
--			Color: Can output a list of colors with the last one beiing dropped.
--			Exs:   init [Red,Green,Blue] => [Red,Green]
--				   init [1,2,3,4] => [1,2,3]
--				   init ["Hello", "there,","!"] => ["Hello","there"]

-- iterate:	Type:  iterate :: (a -> a) -> a -> [a]
--			Info:  Outputs an infinite list that continues the same
--				   calculations with each new answer.
--			Color: The Color function can't be used here.
--			Exs:   take 5 (iterate (2*)1) => [1,2,4,8,16]
--				   take 5 (iterate (5+)5) => [5,10,15,20,25]
--				   take 4 (iterate (2^)1) => [1,2,4,16]

-- last:	Type:  last :: [a] -> a
--			Info:  Outputs the last value of an inputted list.
--			Color: Can output a color if it's the last value of a list.
--			Exs:   last [Red,Green,Blue] => Blue
--				   last [1,2,3,4] => 4
--				   last ["1","2","3","GO!"] => "GO!"

-- lcm:		Type:  lcm :: Integral a => a -> a -> a
--			Info:  Outputs the lowest number that both inputted numbers could
--				   multiply up to.
--			Color: Since this is a function that handles numbers, the Color
--				   function can't be used.
--			Exs:   lcm 5 10 => 10
--				   lcm 9 36 => 36
--				   lcm 5 23 => 115

-- length:	Type:  length :: Foldable t => t a -> Int
--			Info:  Outputs the length of an inputted list.
--			Color: Colors can be used in a list, so the length of a list of
--				   colors could be outputted.
--			Exs:   length [Red,Blue] => 2
--				   length [1,2,3,4,5] => 5
--				   length [] => 0

-- map:		Type:  map :: (a -> b) -> [a] -> [b]
--			Info:  Modifies every value of a list in some way and outputs
--				   the modified list.
--			Color: Colors can't be used since math is mostly used.
--			Exs:   map reverse ["hello","there"] => ["olleh","ereht"]
--				   map (2+) [2,4,6] => [4,6,8]
--				   map (^2) [1,2,3] => [1,4,9]

-- null:	Type:  null :: Foldable t => t a -> Bool
--			Info:  Outputs whether or not the input is empty with True or False.
--			Color: The color function could technically be used, but there
--				   isn't a point to it.
--			Exs:   null [Red] => False
--				   null [] => True
--				   null "" => True

-- odd:		Type:  odd :: Integral a => a -> Bool
--			Info:  Outputs whether or not a number is odd using True or
--				   False.
--			Color: Since colors aren't numbers, this function can't be used.
--			Exs:   odd 3 => True
--				   odd 2 => False
--				   odd 0 => False

-- repeat:	Type:  repeat :: a -> [a]
--			Info:  Repeats a value an infinite amount of times in a list.
--			Color: Colors can be repeated with this function.
--			Exs:   take 5 (repeat Blue) => [Blue,Blue,Blue,Blue,Blue]
--				   take 3 (repeat 1) => [1,1,1]
--				   take 3 (repeat 'C') => "CCC"

-- replicate: Type:  replicate :: Int -> a -> [a]
--			  Info:  Works the same way as repeat but has a limit as to
--					 how many times a value is repeated.
--			  Color: Just like with repeat, the Color function can be
--					 used to replicate colors in a list.
--			  Exs:   replicate 3 Green => [Green,Green,Green]
--				     replicate 5 'A' => "AAAAA"
--				     replicate 2 "Hello" => ["Hello","Hello"]

-- reverse:	Type:  reverse :: [a] -> [a]
--			Info:  Outputs the inputted list, however it's in reverse.
--			Color: A list of colors can be reversed.
--			Exs:   reverse [Red,Blue,Green] => [Green,Blue,Red]
--				   reverse [1,2,3,4,5] => [5,4,3,2,1]
--				   reverse ['a','b','c'] => ['c','b','a']

-- snd:		Type:  snd :: (a, b) -> b
--			Info:  Similar to fst as it accepts a pair for input, but it outputs
--				   the second value instead.
--			Color: Just like with fst, the Color function will work if there's
--				   a color for the second value in a pair.
--			Exs:   snd (Red,Green) => Green
--				   snd ("Yes","No") => "No"
--				   snd (True,False) => False

-- span:	Type:  span :: (a -> Bool) -> [a] -> ([a], [a])
--			Info:  Splits a list into a tuple of lists using a condition, where
--				   the first list contains values where they followed the
--				   condition and the second list contains values that don't.
--			Color: The Color function can be used with the lists and certain
--				   conditions.
--			Exs:   span (==Red) [Red,Green,Blue] => ([Red],[Green,Blue])
--				   span (<0) [-3,-2,-1,0,1,2] => ([-3,-2,-1],[0,1,2])
--				   span (>5) [0,1,2,3] => ([],[0,1,2,3])

-- splitAt:	Type:  splitAt :: Int -> [a] -> ([a], [a])
--			Info:  Splits a list apart at an inputted position, outputting
--				   a tuple of lists.
--			Color: Colors can be used in the lists.
--			Exs:   splitAt 2 [Red,Blue,Yellow,Orange] => ([Red,Blue],[Yellow,Orange])
--				   splitAt 3 [0,1,2,3,4,5] => ([0,1,2],[3,4,5])
--				   splitAt 0 [2,4,6] => ([],[2,4,6])

-- zip:		Type:  zip :: [a] -> [b] -> [(a, b)]
--			Info:  Takes two inputted lists and combines them into one list
--				   composed of pairs.
--			Color: One of the lists could be composed of colors and therefore
--				   zipped with another list.
--			Exs:   zip [1,2,3] [Red,Green,Blue] => [(1,Red),(2,Green),(3,Blue)]
--				   zip [1,3,5] [2,4,6] => [(1,2),(3,4),(5,6)]
--				   zip [1,2,3] ["Yes","No","I don't know"]
--				        => [(1,"Yes"),(2,"No"),(3,"I don't know")]
