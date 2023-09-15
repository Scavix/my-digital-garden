---
dg-publish: true
---
### Backtracking and its importance
The #backtracking is a search and inference strategy used by Prolog to explore multiple possible solutions when trying to satisfy a query or goal. It allows Prolog to backtrack or undo previous choices and explore alternative paths when a particular choice leads to a dead-end or a failure. Prolog uses backtracking to systematically explore the solution space until it finds a valid solution or exhausts all possibilities.
### Prolog control structures.
Prolog control structures include mechanisms like the cut operator (`!`), the fail predicate (`fail`), and conditional statements (`if-then-else`). These control structures are used to control the flow of execution and make decisions within Prolog programs. Let's explore each of these control structures:
**Cut Operator (`!`):**
The cut operator is used to control backtracking and prune search paths. It commits Prolog to a specific choice and prevents it from backtracking to previous choices.
When used, the cut operator "cuts off" alternative solutions that may have been explored during backtracking, essentially saying, "Do not consider other options; this is the solution."
```prolog
member(X, [X | _]) :- !.
member(X, [_ | Tail]) :- member(X, Tail).
```
**Fail Predicate (`fail`):**
The `fail` predicate is used to indicate failure explicitly. It always fails when encountered.
Fail is often used in combination with the cut operator to prune unwanted branches of the search tree.
```prolog
find_positive(X, [X | _]):- X > 0, !.
find_positive(X, [_ | Tail]):- fail.
```
**Conditional Statements (`if-then-else` or `->`):**
The `if-then-else` construct, which is often written as `Condition -> Then ; Else`. It allows you to specify different actions based on a condition.
The `Condition` is evaluated first, and if it succeeds, the `Then` part is executed; otherwise, the `Else` part is executed.
```prolog
is_positive(X):-
	X > 0 -> write('Positive') ; write('Non-positive').
```
### Call
In Prolog, the `call/1` predicate is used to dynamically call another predicate or goal based on a variable or a constructed term. It allows you to create and execute predicates or goals at runtime, which can be particularly useful in situations where you need to apply different predicates based on data or user input. eg:
```prolog
%%% greater_than/2
greater_than(X, Y) :- X > Y.

%%% lesser_than/2
lesser_than(X, Y) :- X < Y.

%%% compare_numbers/3
compare_numbers(X, Y, Result) :-
    X > Y -> call(greater_than(X, Y), Result) ; call(lesser_than(X, Y), Result).
```
### Higher order predicates
In Prolog, `findall/3`, `bagof/3`, and `setof/3` are built-in predicates used for collecting and organizing solutions to queries. These predicates help you retrieve and manipulate sets of solutions based on certain criteria.
**`findall/3`** is used to collect all solutions to a query and store them in a list. It retrieves all instances that satisfy a given goal, regardless of duplicates.
The syntax is: `findall(+Template, +Goal, -List)`.
```prolog
%%% person/1
person(john).
person(mary).
person(jane).

?- findall(X, person(X), People). % true and People = [john, mary, jane].
```
**`bagof/3`** is used to collect solutions to a query into a bag, which allows duplicates. It groups solutions based on a variable or a list of variables.
The syntax is: `bagof(+Template, +Goal, -Bag)`.
```prolog
%%% likes/2
likes(john, pizza).
likes(jane, pizza).
likes(john, ice_cream).
likes(mary, sushi).

?- bagof(Food, Person^likes(Person, Food), Likes). % true and Likes = [pizza, pizza, ice_cream, sushi].
```

[[5 - Working with Files and IO]]