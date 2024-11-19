{-
Add your code to this file after the Problem statements.
The names of the functions SHOULD NOT BE CHANGED. We may grade (parts of) this
assignment with a script - if your code doesn't define the functions with the exact names
specified, they will be assumed skipped. We WILL NOT adjust by hand to account for this.
Give type signatures for all top level functions.

Written answers should be submitted as comments in this file.

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

module HW8 where

import Prelude hiding (lookup, iterate, isSubsequenceOf)

{-
Problem 1:

Write a function "prodAll" that takes a list of Int's and returns their product.
Use pattern matching and recursion to directly manipulate the list.

Example:
...> prodAll [2,7,4]
56
-}

{-
Problem 2:

Define a function composeList which composes a list of unary functions into a 
single function. The empty list should be treated as the identity function.
Use pattern matching and recursion to directly manipulate the list.

Example:
...> composeList [ (*) 2, (*) 2] 2
8
...> composeList [ (-) 3 , (*) 2, (+) 5 ] 7 
-21
Notice how in the above example, the output of composeList [ (-) 3, (*) 2, (+) 5]
is the function f(x) = (3 - (2 * (5 + x))).
-}

{- Problem 3:

Write a function iterate that takes a function (f :: a -> a) and a value (x :: a), and returns an infinite list of repeated applications of f to x

iterate f x == [x, f x, f (f x), f (f (f x)), ...]
-}

{- Problem 4:

Write a function isSubsequenceOf that takes two lists and returns True if all elements of the first list occur, in order, in the second.
The elements do not have to occur consecutively.

Example:
...> isSubsequenceOf [1, 5, 10] [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
True
...> isSubsequenceOf [1, 2, 3] [1, 1, 2]
False
...> isSubsequenceOf [1, 2, 3] [3, 2, 1]
False
-}

{-
Problem 5:
a) What would be a more logical name for the function f, defined below?  

b) What is the computational complexity of f, in terms of the length of the input list?
(++) is defined as part of the Haskell standard library, which is also called the "Prelude".
Assume its implementation is the same as was presented in class.
-}

f :: [a] -> [a]
f [] = []
f (x:xs) = f xs ++ [x]

{-
Problem 6:

Write a definition for the inside function, which returns True if and only if point2 lies inside a circle of radius around point1.

You may add typeclass requirements to inside's type signature, but do not change it in any other way.
-}

data Point a = Point a a deriving Show

inside :: a -> Point a -> Point a -> Bool
inside radius point1 point2 = error "define"

{- Problem 7:

Do & and #, as defined below, return the same result in all cases?
That is, could you replace any occurrence of & with # or any occurrence of # with &
in a program, and obeserve the same behavior of the program?  Explain.
-}

data AB = A | B AB deriving Show

(&) :: AB -> AB -> AB
A & y = y
(B x) & y = B (x & y)

(#) :: AB -> AB -> AB
A # y = y
(B x) # y = x # B y