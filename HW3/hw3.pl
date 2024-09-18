 /* Homework Assignment 3
   Programming Languages
   CS471, Fall 2024
   Binghamton University */

/* Instructions */

/*

This section deals with general submission instructions. Use this
source file. You will be
able to code in and run the file in the prolog interpreter directly. I
recommend reading this assignment directly from the source file.
You will need to rename the source from hw3.txt to hw3.pl.

We will be using swipl for our prolog environment:
 To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw3].
 On the lab computers, you can run swipl by typing `prolog`.

cd PATH_TO_FILE
prolog
[hw3].

From then on you may execute queries (goals) in the prompt.
You should provide your answers in the designated spot. Once you have
added some code to the file, rerun [hw3]. in the swipl prompt to
reload.

In addition, there are unit tests for many of the problems. These are there to
help you better understand what the question asks for, as well as
check your code. They are included in our knowledge base as queries
and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the
initial % for each test for the associated problem and reload the
assignment file ([hw3].). Each SUCCEED line should silently load and
succeed, and each FAIL line should throw a WARNING. If a SUCCEED line
throws a WARNING, or a FAIL line fails to, then you solution is not
correct. If you pass the tests there is a good chance that your code
is correct, but not guaranteed; the tests are meant as guided feedback
and are not a check for 100% correctness.

*/

/*
Collaboration List:
Collaborated with Xinwei on the testcases for the 8th and 9th question, and discussed about a unknown error that occured in 2nd question. 
(cause was didn't save the file before running).


*/

/* Submission */

/*
For this assignment -- and the remaing Prolog assignments -- you must
submit only the source file. Non-code answers may be written in comments.
Coding should be done directly in hw3.pl.
*/

/* Homework 3 */

/* Due: Friday, February 16th, 11:59 PM */

/*
Purpose: To get comfortable with backtracking, recursion,
   become familar with reflective mechanism of Prolog,
   and Prolog as a symbolic programming language.
*/

/* Problem 1:
   Indicate whether each of the following will unify or not.
   If unification will succeed, indicate how each variable will be bound.
   If unification will fail, briefly (a single phrase or sentence) explain why.
   You may write your answers directly in this comment, or in another comment
   below this one.
   You are strongly encouraged to:
      (a) first try and answer each problem by hand
      (b) then check your answer using a prolog interpreter.

   1) (T, T) = (B, C).
   2) (t, t) = (B, C).
   3) (T, T) = (b, c).
   4) [H|T] = [1, 2, 3].
   5) [H|T] = [1].
   6) [H|T] = [].
   7) f(1, X) = f(Y, 2).
   8) r(1, X) = f(Y, 2).
   9) r(1, 2) = r(1, 2, X).
   10) f(g(X, Y), h(Y)) = f(g(r(T), 2), Z).
*/

/* Problem 1 Answer:
   1) (T,T) = (B,C). will unify. The Bindings are T=B and B=C. First the left T is bound to B and then right T is bound to C. But as T is already bound to B, B is bound to C.
   2) (t, t) = (B, C). will unify. The bindings are B = t and C = t. As both B and C didn't have any other bindings they are mapped to left and right t respetively.
   3) (T, T) = (b, c). will not unify. The left T is first bound to b, but as T is now bound to b, this will not unify as T is also being tried to be mapped with c.
   4) [H|T] = [1, 2, 3] will unify. The bindings are H = 1 and T = [2, 3].
   5) [H|T] = [1]. will unify. The bindings are H = 1 and T = [].
   6) [H|T] = []. will not unify. An non empty list, cannot be bound to an empty list.
   7) f(1, X) = f(Y, 2). will unify. The bindings are X = 2, Y = 1.
   8) r(1, X) = f(Y, 2). will not unify as functors are not the same.
   9) r(1, 2) = r(1, 2, X). will not unify as the arity is not the same.
   10) f(g(X, Y), h(Y)) = f(g(r(T), 2), Z). will unify. The bindings are X = r(T), Y = 2, Z = h(2).

*/

/* Problem 2:
   (Exercise 3.5 from Learn Prolog Now!)
   Binary trees are trees where all internal nodes have exactly two children.
   The smallest binary trees consist of only one leaf node. We will represent
   leaf nodes as leaf(Label) . For instance, leaf(3) and leaf(7) are leaf
   nodes, and therefore small binary trees.

   Given two binary trees B1 and B2 we can combine them into one binary tree
   using the functor tree/2 as follows: tree(B1,B2) .
   So, from the leaves leaf(1) and leaf(2) we can build the binary tree
   tree(leaf(1),leaf(2)) .
   From the binary trees tree(leaf(1),leaf(2)) and leaf(4) we can build
   tree( tree(leaf(1), leaf(2)), leaf(4)) .

   Define a predicate swap/2 , which produces the mirror image of the binary
   tree that is its first argument. For example:

   ?-  swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T).
   T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
*/

/* Problem 2 Answer: */
swap(leaf(X), leaf(X)).

swap(tree(X, Y), tree(Y_, X_)):- swap(X, X_), swap(Y, Y_).

/* Problem 2 Test: */
 :- swap( tree( tree(leaf(1), leaf(2)), leaf(4)), T), T  =  tree( leaf(4), tree(leaf(2), leaf(1))).
 :- swap(leaf(1), leaf(1)).
 :- swap(tree(leaf(1), leaf(2)), tree(leaf(1), leaf(2))) -> fail ; true.

/* Problem 3:
   Write a predicate sumlist(List,Sum) which succeeds if Sum is the total value
   of all the elements of List. This will be a top down recursion.
   The recursion clause will add the current value to the result of the sum
   of the rest of the list.
   We have already provided the base case for this predicate underneath
   'Problem 1 Answer'. You just need to add the recursive clause.
*/

/* Problem 3 Answer */

sumlist([], 0).
sumlist([H|T], Sum) :- sumlist(T, Sum1), Sum is Sum1 + H.

/* Problem 3 Test */
/* There should be no warnings when compiling,
   tests which are supposed to fail are written as such */

 :- sumlist([], 0).
 :- sumlist([], 1) -> fail ; true.
 :- sumlist([1,2,3,4], 10).
 :- sumlist([1], 1).

/* Problem 4:
   Write the predicate sumlist2(List,Sum) which succeeds if Sum is the sum total
   of all the elements of List. Instead of adding the current value to the
   result of the sum of the tail, you will calculate the partial sum of the all
   the elements you have reached so far. You will need an extra argument to
   store the partial sum, so you will write an auxilitary predicate sumlist2/3
   to handle the extra argument.
*/

/* Problem 4 Answer */

sumlist2(List, Sum) :- sumlist2(List, 0, Sum).
sumlist2([], X, X).
sumlist2([H|T], Psum, Sum) :- Sum1 is Psum + H, sumlist2(T, Sum1, Sum).


/* Problem 4 Test */

 :- sumlist2([], 0).
 :- sumlist2([], 1) -> fail ; true.
 :- sumlist2([1,2,3,4], 10).
 :- sumlist2([1], 1).

/* Problem 5:
   Write the predicate sumPartialR(N, SumLst), which succeeds as follows:
   given a number N, SumLst is a sequence of sums such that first number in
   S is the sum of all the numbers from N to 1, the second number in S the sum
   of all the numbers from N-1 down to 1, and so on.
   In other words, SumLst = [N+(N-1)+..+1, (N-1)+(N-2)+..+1, ..., 1].
   For example:

     ?- sumPartialR(6,S).
     S = [21, 15, 10, 6, 3, 1] .

   This problem should be solved by writing exactly 2 clauses.
*/

/* Problem 5 Answer */

sumPartialR(1, [1]).
sumPartialR(N, [H|T]):- N > 1, H is N*(N+1)//2, N1 is N - 1, sumPartialR(N1, T).


/* Problem 5 Test */

 :- sumPartialR(1, [1]).
 :- sumPartialR(1, []) -> fail ; true.
 :- sumPartialR(2, [3, 1]).
 :- sumPartialR(6, [21, 15, 10, 6, 3, 1]).

/* Problem 6:
   We will use a predicate edge(X,Y) to encode a graph.
   edge(X,Y) is true if there is a directed edge from X to Y.
   The following is a mini graph encoded in Prolog. */

edge(a,b).
edge(a,f).
edge(a,c).
edge(b,a).
edge(b,c).
edge(b,d).
edge(c,e).
edge(f,e).

/* Using your knowledge of backtracking and the findall predicate, write
   predicates outgoing/2 and incoming/2.

   outgoing(X,Y) should succeed if Y is a list of all immediate vertices
   reached from X's outgoing edges. incoming(X,Y) should succeed if Y is a
   list of all vertices that have outgoing edges to X.

   You can find definitions of graph terms at
    https://en.wikipedia.org/wiki/Glossary_of_graph_theory_terms
*/

/* Problem 6 Answer */

outgoing(X, Y):- findall(Z, edge(X, Z), Y).
incoming(X, Y):- findall(Z, edge(Z, X), Y).

/* Problem 6 Test */
 :- outgoing(a,X), X = [b,f,c].
 :- outgoing(e,X), X = [].
 :- incoming(a,X), X = [b]..
 :- incoming(f,X), X = [a].

 :- outgoing(e,X), X = [a] -> fail ; true.
 :- incoming(e,X), X = [] -> fail ; true.

/* Problem 7:
  A good example of symbolic computation is symbolic differentiation. Below
  are the rules for symbolic differentiation where U, V are mathematical
  expressions, C is a number constant, N is an integer constant and x is a
  variable:

       dx/dx = 1
       d(C)/dx = 0.
       d(Cx)/dx = C
       d(-U)/dx = -(dU/dx)
       d(U+V)/dx = dU/dx + dV/dx
       d(U-V)/dx = dU/dx - dV/dx
       d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
       d(U^N)/dx = N*U^(N-1)*(dU/dx)

  Translate these rules into Prolog. (Please keep the order of the rules the
  same for testing purposes).

  You can use number/1, to check if a value is a number.
*/

/* Problem 7 Answer: */
d(x, 1):- !.
d(C, 0):- number(C), !.
d(C*x, C):- number(C), !.
d(-U, -DU):- d(U, DU), !.
d(U+V, DU + DV):- d(U, DU), d(V, DV), !.
d(U-V, DU - DV):- d(U, DU), d(V, DV), !.
d(U*V, U*DV + V*DU):- d(U, DU), d(V, DV), !.
d(U^N, N*U^(N1)*DU):- number(N), N1 is N-1, d(U, DU), !.


/* Problem 7 Test: */

 :- d(x,R), R = 1 .
 :- d(7*x,R), R = 7 .
 :- d(x +2*(x^3 + x*x),Result), Result = 1+ (2* (3*x^2*1+ (x*1+x*1))+ (x^3+x*x)*0) .
 :- d(-(1.24*x -x^3),Result), Result = - (1.24-3*x^2*1) .
 :- d(-(1.24*x -2*x^3),Result), Result = - (1.24- (2* (3*x^2*1)+x^3*0)) .

% Pay careful attention to why this fails.
 :- d(x +2*(x^3 + x*x),Result), Result = 1+ (2* (3*x^(3-1)*1+ (x*1+x*1))+ (x^3+x*x)*0) -> fail ; true.

/* Problem 8:
  (Adapated from Prolog Minimanual)
  The towers of Hanoi is a classic puzzle.  In the puzzle, there are three
  pegs (A, B, and C).  Peg A has a stack of successively smaller disks
  on it.  The objective is to move the stack from peg A to peg C, such that:
    1) Disks may only be moved from peg to peg.
    2) Only one disk may be moved at a time.
    3) The only disks that may be moved are the top disks on the pegs.
    4) A disk may only be placed on the base level, or on a larger peg.

  You may enjoy trying to work out a technique to move the pegs yourself, so
  this is a spoiler warning for a solving technique below.

  Solving:
    You can solve the towers of Hanoi with this recursive algorithm: if N is the
    number of disks:
      1) move N - 1 disks from the source peg to the spare peg
      2) move the Nth disk from the source peg to the destination peg
      3) Finally, move the N - 1 disks from the spare peg to the destination peg
    Consider: how is this procedure recursive?  What is the base case?  How do
    you know when you are finished?

    The below code is the start of prolog program to solve the towers of Hanoi
    puzzle for an arbitrary N. hanoi(N) will print the solution.  report prints
    the required steps.  move should recursively call itself and call report to
    output the steps.  However, only the base case of move is provided.
    Provide a recursive case for move so that hanoi(N) correctly solves the
    towers of Hanoi puzzle.  This recusive case should call report, and
    (by definition of recursion) call move at least once.
*/

/* Problem 8 Answer: */

hanoi(N) :- move(N, source, destination, spare).

report(X, Y) :-
    write('Move the top disk from the '),
    write(X),
    write(' peg to the '),
    write(Y),
    write(' peg.').

move(0, _, _, _) :- !.
move(N, Src, Dest, Spare):- N>0, N1 is N-1, move(N1, Src, Spare, Dest), report(Src, Dest), move(N1, Spare, Dest, Src).


/* Problem 9:
The Ackermann function, named for and invented by Wilhelm Ackermann, is one of
the earliest discoved total non-primitive recursive functions.  A total function is a
function that is defined on every element of it's input domain (in this case, the natural
numbers.)  A primitive recursive function is, roughly, a function that can be written as a
for loop- that, is the number of required recursive calls has an upper bound.

One variation of the Ackermann function is 
  ack(m, n) = n + 1                        if m = 0
  ack(m, n) = ack(m - 1, 1)                if n = 0 and m > 0 
  ack(m, n) = ack(m - 1, ack(m, n - 1))    if n >0 and m > 0

(There are several different variants of this function.
All are called Ackermann's functions and their values do not necessarily agree!)

The Ackermann Number N of a compiler is the largest N for which
          ack (3,N) 
gives an answer without a stack overflow.  In earlier decades a variation had been used as
a of the benchmarking algorithms.  

(a) Define a prolog relation ack/3.  Determine (by testing) the Ackermann Number of your prolog intepreter

(b) Define a prolog relation ackmemo/3.  This relation should make use of memoization, as discussed in class.
Based on your observations, how does performance of ackmemo compare to ack?
*/

/* Problem 9a Answer: */

ack(0, N, O):- O is N + 1, !.
ack(M, 0, O):- M > 0, M1 is M - 1, ack(M1, 1, O), !.
ack(M, N, O):- N > 0, M > 0, M1 is M - 1, N1 is N - 1, ack(M, N1, O1), ack(M1, O1 , O), !.


/* Problem 9b Answer: */

:- dynamic  lookup/3.
ackmemo(M, N, O):- lookup(M, N, O), !.

ackmemo(0, N, O):- O is N + 1, assert(lookup(0, N, O)), !.
ackmemo(M, 0, O):- M > 0, M1 is M - 1, ackmemo(M1, 1, O), assert(lookup(M, 0, O)), !.
ackmemo(M, N, O):- N > 0, M > 0, M1 is M - 1, N1 is N - 1, ack(M, N1, O1), ack(M1, O1 , O), assert(lookup(M, N, O)) , !.

/* ackmemo is faster than ack especially when the numbers become bigger, as memoization helps in reducing the calculations. */