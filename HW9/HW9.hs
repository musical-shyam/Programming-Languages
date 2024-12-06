{-
Add your code to this file in the positions indicated with "Problem N Answer:".
The names of the functions SHOULD NOT BE CHANGED. We may grade (parts of) this
assignment with a script - if your code doesn't define the functions with the exact names
specified, they will be assumed skipped. We WILL NOT adjust by hand to account for this.
Give type signatures for all top level functions.

Once you've finished editing this file, submit on BrightSpace as normal.
-}

{-
Running instructions:

The lab computers have GHC 8.8.4 installed.  GHC is the most widely used Haskell comiler.
It also comes with an intepreter ghci, which makes it easy to quickly test Haskell code
(this is what I've been using to demo Haskell in class.)
To start ghci, start the command line and type:
    ghci
To (re)load this file type:
    :l HW8.hs

If you want to install GHC on your personal computer, there are a number of options here:
https://www.haskell.org/downloads/

Keep in mind that submissions are required to work on the lab computers, so even if you are
working on your personal computer, run a quick test in the lab before submission.
-}

{-
Colaboration List: 
colaborated with noone. used internet sources.
https://learnyouahaskell.com/chapters
https://mathworld.wolfram.com/Ring.html
https://hoogle.haskell.org/
https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

-}

module HW9 where

import Prelude hiding (map, filter, lookup)

{-
Problem 1:

Consider the below Map data structure, which allows mapping keys k to values v.
Implement insert, to insert key/value pairs into the Map.
Implement lookup, to lookup the value of a particular key (or nothing, if that key is not
in the Map.)
Design your functions to use a binary search.

You may add typeclass requirements to the type signatures, but do not change them in any other way.
-}

data Map k v = Branch (Map k v) k v (Map k v) | Leaf deriving Show

insert :: Ord k => k -> v -> Map k v -> Map k v
insert key value Leaf = Branch Leaf key value Leaf
insert key value (Branch left k v right)
    | key < k = Branch (insert key value left) k v right
    | key > k = Branch left k v (insert key value right)
    | otherwise = Branch left key value right


-- *HW9> insert 5 6 (insert 9 6 (insert 4 12 (insert 7 1 Leaf)))
-- Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup key (Branch left k v right)
    | key < k = lookup key left
    | key > k = lookup key right
    | otherwise = Just v


-- *HW9> lookup 9 (Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf))
-- Just 6
-- *HW9> lookup 1 (Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf))
-- Nothing

{-
Problem 2:

Fill in the below Functor and Foldable instances for Map.
Keep in mind that the intializer passed to a fold function should be used exactly once.
When folding, the values in the Map should be passed to the combining operator
in descending order of their associated keys (assuming a valid binary tree.)
You may find it helpful to define a helper function to implement Foldable.
-}

instance Functor (Map k) where
    fmap _ Leaf = Leaf
    fmap f (Branch left k v right) =
        Branch (fmap f left) k (f v) (fmap f right)

-- *HW9> fmap (+ 1) (Branch (Branch Leaf 4 8 Leaf) 9 6 (Branch (Branch Leaf 7 21 Leaf) 8 5 Leaf))                          
-- Branch (Branch Leaf 4 9 Leaf) 9 7 (Branch (Branch Leaf 7 22 Leaf) 8 6 Leaf)

desc :: Map k v -> [v]
desc Leaf = []
desc (Branch left _ v right) = desc right ++ [v] ++ desc left

instance Foldable (Map k) where
    foldr f acc tree = foldr f acc (desc tree)


-- *HW9> foldr (-) 0 (Branch (Branch Leaf 4 8 Leaf) 6 6 (Branch (Branch Leaf 7 21 Leaf) 8 5 Leaf))  
-- 18

{-
Problem 3:

Hoogle is a search engine for Haskell functions:
    https://hoogle.haskell.org
You can enter function names, type names, or typeclass names into Hoogle to find those functions.
You can also enter types, and find functions with those types.  For example, if you enter:
    (p -> q) -> [p] -> [q]
Hoogle will find the function:
    map :: (a -> b) -> [a] -> [b]
(and a large number of slight variants on map.)

The Applicative typeclass extends Functors to allow applying functions wrapped in a Functor.
Look up Applicative using Hoogle.

Also note the laws for the Applicative typeclass. You will need to consider these laws to answer
the following question.

Consider the below definition of an Applicative instance for Map.
What is wrong with this definition of Applicative?  Is it fixable?  Explain.

Write your answer in a comment.
-}

instance Ord k => Applicative (Map k) where
    pure x = Leaf
    m1 <*> m2 = foldr f Leaf (keyValPairs m2) 
        where
            f (k, v) m = case lookup k m1 of
                                Just f -> insert k (f v) m
                                Nothing -> m

keyValPairs :: Map k v -> [(k, v)]
keyValPairs (Branch l k v r) = keyValPairs l ++ (k, v):keyValPairs r
keyValPairs Leaf = []

{-
Answer 3:
An Applicative instance must satisfy Identity, Composition, Homomorphism and Interchangability laws. It should also include the implementation of pure and <*>. 
The definition of the instance in the code above has several issues. 

1. the pure is defined as Leaf. This is wrong because pure should implement a structure that wraps over the value x (so that x can be used in the function application). Here Leaf has no values by itself, and it means that it is generating a empty tree. 

2. The problem with <*> is that it assumes that all the keys in m1 aligns with m2. if they don't align, the function application fails for those keys and the data is dropped. 

Because of these issues the law of identity is violated. pure id <*> v will fail because pure id would produce a Leaf, and applying leaf to any map will result in Leaf thus erasing whatever is there in v. 

pure f <*> pure x would result always in Leaf (it should return a map containing the result pure(f x)), thus the law of Homomorphism is also violated.

The interchange law is also violated, pure (.) <*> u <*> v <*> w = u <*> (v <*> w). Here the LHS always results in Leaf, while the RHS need not be Leaf.

Is it Fixable? How to Fix it?
For Map k v you always need a key to store values. without a key, you can't just wrap a value. The Applicative expects pure to wrap a value without requiring a key and it doesn't provide a way to introduce keys. The applicative instance requires a consistent structure, but map inherently requires keys for all operations thus making map incompatible with Applicative Laws. Thus with the current definition of map, it is not fixable.


-}

{-
Problem 4:

Both map and filter can be defined by passing the correct function and initalizer to foldr.
Fill in the below two definitions to define map and filter using foldr.
-}

map :: (a -> b) -> [a] -> [b]
map f = foldr(\x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr(\x acc -> if p x then x : acc else acc) []

{-
Some Test Cases:
map (++ "!") ["Hello", "Hi", "Bye"]
-- ["Hello!", "Hi!", "Bye!"]

map (+1) [1, 2, 3]
--[2, 3, 4]

filter even [1, 2, 3, 4, 5, 6]
-- [2, 4, 6]

filter (const True) [1, 2, 3, 4]
-- [1, 2, 3, 4]

-}

{- Problem 5:

Peano numbers are a way of representing the natural numbers
using a Zero constructor and a Succ (or Successor) constructor.

0 is represented as Zero.
1 is represented as (Succ Zero).
2 is represented as (Succ (Succ Zero)).
3 is represented as (Succ (Succ (Succ Zero))).
And so on...


(a) Use a deriving clause to get Eq, Ord, and Show instances for the below definition of Peano numbers.
Test the instances.  What bug do you notice?  Adjust the data definition to fix the bug.

(b) Using Hoogle, look up the Enum typeclass.  Define (by hand) an instance of Enum for Peano.
(In practical cases, Enum can be derived, just like Eq or Ord.)
You may assume that toEnum is passed a non-negative Int.

(c) Define an instance of Num for Peano.

Strictly speaking, Num does not have any laws, but often instances are expected to form a Ring.
    https://mathworld.wolfram.com/Ring.html
Is this possible in the case of Peano?  Are there compromises or decisions you had to make in
defining the Num instance?  Explain in a comment.

(d) Define an instance of Foldable for Peano, or explain why defining an instance of
Foldable for Peano is not possible/does not make sense. 
-}

data Peano = Zero | Succ Peano
    deriving (Eq, Ord, Show)

instance Enum Peano where
    toEnum 0 = Zero
    toEnum x = Succ (toEnum(x-1))
    fromEnum Zero = 0
    fromEnum (Succ p) = 1 + fromEnum p

instance Num Peano where
    Zero + n = n 
    (Succ m) + n = Succ (m + n)

    Zero * _ = Zero
    (Succ m) * n = n + (m * n)

    abs p = p 

    signum Zero = Zero 
    signum (Succ _) = Succ Zero

    fromInteger x
        | x < 0 = error "negative not supported by Peano"
        | x == 0 = Zero
        | otherwise = Succ (fromInteger(x-1))
    
    negate Zero = Zero
    negate _ = error "no negative representation for non-zero Peano"



{- Comments for all sub parts of Question 5

(a) Deriving Instances for Peano
data Peano = Succ Peano | Zero deriving (Eq, Ord, Show)
There was wrong results when testing ord. By default Haskell uses the order of constructors to determine Ord. According to the derived Old instance succ something < Zero, cause Succ is listed first. but this should not be the case, cause Zero should be the one that is smaller.

Test Cases:
let one = Succ Zero
let two = Succ (Succ Zero)

print one
-- Succ Zero
print two
-- Succ (Succ Zero)

one == two
-- False
one == Succ Zero
-- True

one < two 
-- True (False intially)
Zero < one 
-- True (False intially)
two < Zero 
-- True (False intially)

(b) Assuming toEnum is non negative.
Test Cases:
toEnum 3 :: Peano    
-- Succ (Succ (Succ Zero))

fromEnum (Succ (Succ Zero))  
-- 2
(c) Defining Num for Peano

Although the Num is expected to have a Ring, we cannot form a ring because Peano cannot represent negative numbers (for example we cannot find additive inverse for all Peano numbers). So here we have to make the comprise to leave negate as a partial operation that only works for Zero and not any other value.

Test Cases:

let three = fromInteger 3 :: Peano    
let two   = fromInteger 2 :: Peano

three + two     -- 5 Peano 
-- (Succ (Succ (Succ (Succ (Succ Zero)))))
three * two     -- 6 Peano 
-- (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

abs three       
-- Succ (Succ (Succ Zero))

signum Zero     
-- Zero
signum three    
-- Succ Zero

negate Zero     
-- Zero
negate three    
-- Exception error

(d) Defining a Foldable Instance for Peano.

Foldable is intended to be used for data structure that hold multiple values of some element type. But Peano does not store multiple values of a parameter type, it is simply a unary representation of a single natural number. It doesn't contain a collection of elements to fold over. so it doesn't make any sense to define Foldable for Peano.

-}