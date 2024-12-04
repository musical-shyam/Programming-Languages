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

insert :: k -> v -> Map k v -> Map k v
insert = error "define"

-- *HW9> insert 5 6 (insert 9 6 (insert 4 12 (insert 7 1 Leaf)))
-- Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf)

lookup :: k -> Map k v -> Maybe v
lookup = error "define"

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

-- *HW9> fmap (+ 1) (Branch (Branch Leaf 4 8 Leaf) 9 6 (Branch (Branch Leaf 7 21 Leaf) 8 5 Leaf))                          
-- Branch (Branch Leaf 4 9 Leaf) 9 7 (Branch (Branch Leaf 7 22 Leaf) 8 6 Leaf)


instance Foldable (Map k) where

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
Problem 4:

Both map and filter can be defined by passing the correct function and initalizer to foldr.
Fill in the below two definitions to define map and filter using foldr.
-}

map :: (a -> b) -> [a] -> [b]
map = error "define"

filter :: (a -> Bool) -> [a] -> [a]
filter = error "define"

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

data Peano = Succ Peano | Zero