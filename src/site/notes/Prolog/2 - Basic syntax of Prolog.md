---
{"dg-publish":true,"permalink":"/prolog/2-basic-syntax-of-prolog/","noteIcon":""}
---

### Atoms
A #atom is used to represent fixed and unchanging value, such as a name, label, or symbol. Atoms are typically written in lowercase letters, enclosed in single quotes, but they can also be written without quotes if they start with a lowercase letter and don't contain any special characters.
```prolog
% This is a comment
% These are all atoms
'apple', 'john_doe', '42', apple, john_doe, 42
```
### Facts
A #fact is a piece of information about the domain, the syntax consists of a predicate followed by parenthesis and arguments, and a dot to finish the statement. eg:
```prolog
% Best practice: write the name of the predicate followed by / and arity

%%% human/1
human(socrates).

%%% likes/2
likes(john, pizza).

% mortal/1
mortal(X) :- human(X).
```
In the snippet, "mortal(X)" is the head, which states that X is mortal if the condition in the body is satisfied. The condition in the body is "human(X)," which means that X must be a human for X to be considered mortal. So, if X is a human, Prolog will infer that X is mortal.
Rules can have "more conditions", we can a comma to separate them, and the condition will be in a "AND" state, where all have to be true. eg:
```prolog
% bigger_than_five/1
bigger_than_five(X):- X>5.

?- bigger_than_five(9). %% true
?- bigger_than_five(3). %% false
?- bigger_than_five(banana). %% error, solution?

safer_bigger_than_five(X):-
	X>5,
	integer(x).

?- bigger_than_five(banana). %% false
```
### Unification, is/2 and return values
The #unification is the process of finding values for variables that make two or more terms equal. Prolog does not really return values, but answers to questions. That is why it may be important sometimes to use well unification to solve problems.
We already saw unification in action, but it is possible to use a "explicit" unification that could be useful in some scenarios (using the unification operator =/2). eg
```prolog
bigger_than_five(X,Y):-
	X>5,
	Y = 'it is bigger than 5'.

?- bigger_than_five(6,Y). % true and Y = 'it is bigger than 5'
```
Arithmetic operations are typically performed using the `is` predicate. The `is` predicate is used to evaluate arithmetic expressions and assign the result to a variable. This is necessary because arithmetic expressions are treated as atoms. eg:
```prolog
%%% square1/2
square1(X,Y):- Y = X**2.

?- square(5,X). %% true and X = 5**2

%%% square2/2
square2(X,Y):- Y is X**2.

?- square(5,X). %% true and X = 25
```
### Wildcard
A #wildcard is often referred to as "anonymous variable." Anonymous variables are represented by an underscore `_` and are used when you want to ignore or don't care about the specific value of a variable in a query or rule. eg:
```prolog
likes(john, laura).

?- likes(john, X). % true and X = laura
?- likes(john, _). % true
```

[[Prolog/3 - Lists and recursion in Prolog\|3 - Lists and recursion in Prolog]]