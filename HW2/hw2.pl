 /* Homework Assignment 2
   Programming Languages
   CS471, Spring 2023
   Binghamton University */

/* Instructions */

/*

This section deals with general submission instructions. Use this source file. You will be
able to code in and run the file in the prolog interpreter directly.

We will be using swipl for our prolog environment:
 To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw2].
 On the lab computers, you can run swipl by typing `prolog`.

cd PATH_TO_FILE
prolog
[hw2].

From then on you may execute queries (goals) in the prompt.
You should provide your answers in the designated spot. Once you have
added some code to the file, rerun [hw2]. in the swipl prompt to
reload.

In addition, there are unit tests for each problem. These are there to
help you better understand what the question asks for, as well as
check your code. They are included in our knowledge base as queries
and are initially commented out -- % is a Prolog line comment.
Lines marked as SUCCEED should be satisfied by your defined relation(s).
Lines marked as FAIL show cases which should not be satisfied.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3)-> fail ; true.   % FAIL

After you have finished a problem and are ready to test, remove the
initial % for each test for the associated problem and reload the
assignment file ([hw2].). Each line should silently load and
succeed. If any line throws a WARNING, then you solution is not
correct. If you pass the tests there is a good chance that your code
is correct, but not guaranteed; the tests are meant as guided feedback
and are not a check for 100% correctness.

This file is named hw2.txt because BrightSpace blocks uploading of .pl files.
You will likely need to rename it to hw2.pl in order to load it in a Prolog
interpreter, and then rename it back to hw2.txt in order to submit.

*/

/* Submission */

/*
For this assignment -- and the remaining Prolog assignments -- you must
submit only the source file. Non-code answers may be written in comments.
Coding should be done directly in hw2.pl.
*/

/* Homework 2 */

/* Due: Friday, September 11th, 11:59 PM */

/*
Purpose: To get comfortable with Logic programming, and get a good
grasp on list manipulation in Prolog.
*/

/* Problem 1a:
   Programming with matching.  A line can be defined by 2 points. A point has an
   x and y coordinate.   A line is vertical if both points have the same x value.
   A line is horizontal if both points have the same y values.  The following
   is a knowledge base which specify what is meant for a line to be vertical
   or horizontal respectively. This example is due to Ivan Bratko.
*/

vertical(line(point(X,_),point(X,_))).
horizontal(line(point(_,Y),point(_,Y))).

/* 1. List the clauses, predicates, rules, and facts.
   2. List the atoms, variables, and data structure constructors. */

/* Problem 1b:
    Peano arithmetic is a way of writing numerals, which is sometimes used in mathematical logic.
    This system makes use of just four symbols: 0, succ, and the left and right parentheses.
    The following is the knowledge base for this representation of a numeral.
    The predicate 'add' is the definition of adding this representation of numbers.
    */

numeral(0).
numeral(succ(X)) :- numeral(X).

add(0,Y,Y).
add(succ(X),Y,succ(Z)) :- add(X,Y,Z).


/* 1. Name the clauses, predicates, rules, and facts.
   2. Name the atoms, variables, and data structure constructors. */

/* Problem 2:

The following are two basic predicates for list manipulation:
my_first/2 and my_last/2. We may refer to a predicate by writings it
as name/arity; hence, my_first/2 means a predicate named my_first with
two arguments.

my_first(X,Y) succeeds if X is the first element of list Y.
my_last(X,Y) succeeds if X is the last element of list Y.

Write definitions for my_first and my_last.
*/

/* Problem 2 Test: */
%:- my_first(a, [a]). % SUCCEED
%:- my_first(3, [3, 2, 1]). % SUCCEED
%:- my_first(3, [4, 3, 2, 1]) -> fail ; true. % FAIL
%:- my_last(a, [a]). % SUCCEED
%:- my_last(1, [3, 2, 1]). % SUCCEED
%:- my_last(1, [3, 2, 1, 0]) -> fail ; true. % FAIL

/* Problem 3:
 Write a predicate init(All, BLst) that succeeds if Blst has all the items of ALL
 except the last item.  The items in BLst are in the same order as ALL.
*/

/* Problem 3 Test: */
% :- init([1], []).       % SUCCEED
% :- init([1,2,3], [1,2]).% SUCCEED
% :- init([1,2], [1,2]) -> fail ; true.  % FAIL
% :- init([1,2], [2]) -> fail ; true.  % FAIL

/* Problem 4:
Write a predicate is_decreasing(X) that succeeds if X is a list of decreasing numbers -- Each number is either the same or lower than the preceding number.

NOTE: You may match two elements at a time against a list: [X,Y|Xs] = List. It's preferable to do it in the rule head however...
some_rule([X,Y|Xs]) :- ...
   X is the first element, Y is the second element, Xs is the rest of the list. */

/* Problem 5 Test: */
%:- is_decreasing([]).            % SUCCEED
%:- is_decreasing([10]).          % SUCCEED
%:- is_decreasing([10,9]).        % SUCCEED
%:- is_decreasing([10,9,7]).      % SUCCEED
%:- is_decreasing([10,9,7,7,2]).  % SUCCEED
%:- is_decreasing([1,1,1,1,1]).   % SUCCEED

%:- is_decreasing([10,9,7,9]) -> fail ; true.    % FAIL
%:- is_decreasing([2,3,1]) -> fail ; true.       % FAIL
%:- is_decreasing([1,2,3]) -> fail ; true.       % FAIL
%:- is_decreasing([7,19])-> fail ; true.        % FAIL

/* Problem 5:
Write a predicate element_at(X,Y,N) that succeeds if X is the Nth element of list Y. Y is 0-indexed.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* Problem 5 Test: */
%:- element_at(3,[1,2,3],2).   % SUCCEED
%:- element_at(1,[1,2,3],0).   % SUCCEED

%:- element_at(1,[1,2,3],1) -> fail ; true.     % FAIL

/* Problem 6:
Write a predicate insert_at(E,Y,N,Z) that succeeds if Z is the list Y with E inserted at index N -- Insert X at index N in Y.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

insert_at(3,[1,2,3],2,[1,2,3,3]).

/* Problem 6 Test: */
%:- insert_at(3,[1,2,3],2,[1,2,3,3]).  % SUCCEED
%:- insert_at(1,[1,2,3],0,[1,1,2,3]).  % SUCCEED
%:- insert_at(a,[1,2,3],1,[1,a,2,3]).  % SUCCEED

%:- insert_at(1,[1,2,3],0,[1,2,3]) -> fail ; true.    % FAIL


/* Problem 7 :
Write a predicate delete_at(E,Y,N,Z) that succeeds if Z is the list Y with E delete at index N -- Delete E at index N in Y.
YOU MUST USE the predicate defined in the above problem to solve this problem.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Z.  */

/* Problem 7 Test: */

%:- delete_at(3,[1,2,3,3],2,[1,2,3]).  % SUCCEED
%:- delete_at(1,[1,1,2,3],0,[1,2,3]).  % SUCCEED
%:- delete_at(a,[1,a,2,3],1,[1,2,3]).  % SUCCEED

%:- delete_at(1,[1,2,3],0,[1,2,3]) -> fail ; true.    % FAIL

/* Problem 8:
Write a predicate zip(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together.

For example...
zip([1,2,3],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip([1],[a],Zs) should give Zs = [(1,a)]

NOTE: You may assume X and Y have the same length. */

/* Problem 8 Test: */
%:- zip([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip([],[],[]).                      % SUCCEED
%:- zip([1],[2],[(1,2)]).               % SUCCEED

%:- zip([1],[2],[(2,3)]) -> fail ; true.               % FAIL
%:- zip([1],[2,3],[(1,2)]) -> fail ; true.             % FAIL

/* Problem 9:

Write a predicate zip2(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together. However, the length of Zs will be equal to the length of Xs or Ys which ever is less.

For example...
zip2([1,2,3,4],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip2([1],[a,b],Zs) should give Zs = [(1,a)] */

/* Problem 9 Test: */
%:- zip2([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip2([],[a,b,c],[]).                  % SUCCEED
%:- zip2([1,3],[],[]).                    % SUCCEED
%:- zip2([1,3],[2],[(1,2)]).              % SUCCEED

%:- zip2([1],[2],[(2,3)]) -> fail ; true.                 % FAIL
%:- zip2([1],[a,b],[(1,a),(1,b)]) -> fail ; true.         % FAIL

/* Problem 10:
   See Problem 1b above for the knowledge base used for defining peano numbers.
   Define a predicate greater_than/2 that takes two peano numbers
   as arguments and decides whether the first one is greater than the second one.
*/

/* Problem 10 Test: */
% :- greater_than(succ(succ(succ(0))),succ(0)).        % SUCCEED
% :- greater_than(succ(succ(0)),succ(succ(succ(0)))) -> fail ; true.  % FAIL



/* Problem 11:
   See Problem 1b above for the knowledge base used for defining subtract/3 .
   Define substract(Num1,Num2,Result) to succeed if Result is the result of
   Num1 - Num2.  Num1, Num2 and Result use four symbols: 0, succ , and the left and right parentheses
   to represent numbers.

   Use the add/3, from problem 1b, definition to define subtract/3.  Do not
   write a recursive definition for subtract/3.
*/


/* Problem 11 Test: */
% :- subtract(succ(succ(0)), succ(0), succ(0)).       % SUCCEED
% :- subtract(succ(succ(0)), 0, succ(succ(0))).       % SUCCEED
% :- subtract(succ(succ(0)), succ(succ(0)), 0).       % SUCCEED
% :- subtract(succ(succ(0)), 0, 0) -> fail ; true.             % FAIL
% :- subtract(succ(succ(0)), succ(0), succ(succ(0))) -> fail ; true. % FAIL

/* Problem 12:
 
Write a predicate has_subseq(X,Y) that succeeds if Y is a list that is a subsequence of a list X. 

For example...
has_subseq([a,b,c,d],[b,d]) should succeed, but has_subseq([a,b,c,d],[b,e]) should fail. */

/* Problem 12 Test: */
%:- has_subseq([a,g,b,d],[g,b]).     % SUCCEED
%:- has_subseq([1,2,3,4],[2,4]).     % SUCCEED
%:- has_subseq([1,2,3,4],[2,3]).     % SUCCEED
%:- has_subseq([1,2,3,4],[]).        % SUCCEED

%:- has_subseq([1,2,3,4],[2,5]) -> fail ; true.     % FAIL
%:- has_subseq([1,2,3,4],[4,3]) -> fail ; true.     % FAIL

/* Problem 13:
 
Write a predicate bubblesort(X,Y) that succeeds if Y is X, sorted from least to greatest.  Items do not need to be unique.

The implementation of bubblesort should be based on the bubble sort algorithm

Hint: It might be helpful to define a helper relation bubble, in addition to bubblesort itself */

/* Problem 13 Test: */
%:- bubblesort([],[]).     % SUCCEED
%:- bubblesort([4, 3, 2, 1],[1, 2, 3, 4]).     % SUCCEED
%:- bubblesort([4, 3, 2, 1, 4],[1, 2, 3, 4, 4]).     % SUCCEED

%:- bubblesort([4, 3, 2, 1],[1, 2, 4, 3]) -> fail ; true.     % FAIL

/* Problem 14:
Write a predicate merge(A,B,M) that succeed if the list M has all the items from lists A and B in increasing order.  Items do not need to be unique.

Hint: You may use predicates defined in previous problems or write helper predicates to aid in solving this problem.
 */

/* Problem 14 Test: */
%:- merge([10,3,2],[11,5,2],[2,2,3,5,10,11]) .       % SUCCEED
%:- merge([0],[],[0]).                               % SUCCEED
%:- merge([],[3],[3]).                               % SUCCEED

%:- merge([3,4],[3],[3]) -> fail ; true.                            % FAIL
