---
dg-publish: true
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

%%% address/5
address(street(Marconi), number(500), zipcode(40124), city(bologna), country(italy)).
```
In the first fact, human is the predicate and socrates is the argument.
Facts can be used to represent various types of information, such as relationships, properties, and attributes.
### Variables
A #variable is used to represent a unknown value or as a placeholders in queries and rules. Variables are represented with uppercase letters or words starting with an uppercase letter.
```prolog
pizza   % atom
Pizza   % variable
```
### Rules
A #rule is used to express a relationship, a condition, or logical implications based on the facts and other rules in the knowledge base. It consists of a head and a body, separated by the `:-`. eg:
```prolog
%%% mortal/1
mortal(X) :- human(X).
```
In the snippet, "mortal(X)" is the head, which states that X is mortal if the condition in the body is satisfied. The condition in the body is "human(X)," which means that X must be a human for X to be considered mortal. So, if X is a human, Prolog will infer that X is mortal.
Rules can have "more conditions", we can a comma to separate them, and the condition will be in a "AND" state, where all have to be true. eg:
```prolog
%%% is_alive/2
is_alive(X,Y):-
	born_after_1900(X),
	human(X).
```
Here we assume that someone is alive, if a person is both a human and born after 1900.
### Queries
A #query is used to retrieve information or find a solution to logical statements based on the facts and rules defined in the knowledge base.
**Ex 1** Given the following facts
```prolog
human(socrates).
human(aristotle).
human(platos).
```
We ask the following queries:
```prolog
?- human(wittgenstein).

?- human(platos).

?- human(X).
```
The first will respond with false, the second with true. This is because our "knowledge base" contains the fact that Platos is a human.
In the third one, there is a variable, hence we are asking "for someone", the program will response with true, and "match" `X` with socrates (as it is the first facts). This is not the only possible solution, it is in fact (badum tss) possible to continue the computation, the program will then match aristotle, and also platos if ran furthemore.
**Ex 2** Given the following fact and rule
```prolog
human(socrates).

mortal(X) :- human(X).
```
We ask the following queries:
```prolog
?- mortal(wittgenstein).

?- mortal(platos).
```
The first will respond with false, the second with true. This is because our knowledge base contains the fact that Platos is a human, again. The predicate `mortal/1` wants to match the argument with someone who is mortal, in our knowledge base platos is mortal, but not wittgenstein, hence the result.
### Arithmetic expressions
Prolog provides standard arithmetic operators for basic operations. Here are the common arithmetic operators:
- `+` (Addition)
- `-` (Subtraction)
- `*` (Multiplication)
- `/` (Division)
- `//` (Integer Division)
- `mod` (Modulo)
- `**` (Exponential)
### Comparisons
You can use arithmetic expressions to make numeric comparisons within rules and queries. Common comparison operators include `<`, `>`, `<=`, `>=`, `=:=` (equality), and `=\=` (inequality).
```prolog
%%% bigger_than_five/1
bigger_than_five(X):- X>5.

?- bigger_than_five(9).         % true
?- bigger_than_five(3).         % false
?- bigger_than_five(banana).    % error, solution?

safer_bigger_than_five(X):-
	X>5,
	integer(x).

?- bigger_than_five(banana).   % false
```
### Unification, is/2 and return values
The #unification is the process of finding values for variables that make two or more terms equal. Prolog does not really return values, but answers to questions. That is why it may be important sometimes to use well unification to solve problems.
We already saw unification in action, but it is possible to use a "explicit" unification that could be useful in some scenarios (using the unification operator =/2). eg
```prolog
%%% bigger_than_five/2
bigger_than_five(X,Y):-
	X>5,
	Y = 'it is bigger than 5'.

?- bigger_than_five(6,Y).   % true and Y = 'it is bigger than 5'
```
Arithmetic operations are typically performed using the `is` predicate. The `is` predicate is used to evaluate arithmetic expressions and assign the result to a variable. This is necessary because arithmetic expressions are treated as atoms. eg:
```prolog
%%% square1/2
square1(X,Y):- Y = X**2.

?- square(5,X).   % true and X = 5**2

%%% square2/2
square2(X,Y):- Y is X**2.

?- square(5,X).   % true and X = 25
```
### Wildcard
A #wildcard is often referred to as "anonymous variable." Anonymous variables are represented by an underscore `_` and are used when you want to ignore or don't care about the specific value of a variable in a query or rule. eg:
```prolog
likes(john, laura).

?- likes(john, X).   % true and X = laura
?- likes(john, _).   % true
```

[[3 - Lists and recursion in Prolog]]