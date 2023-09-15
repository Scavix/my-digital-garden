---
dg-publish: true
---
### List syntax
A #list is a fundamental data structure used to represent collections of elements. Lists can contain a mix of constants, variables, and even other lists. Lists are enclosed in square brackets `[...]`, and elements within a list are separated by commas.
```prolog
MyList = [apple, 42, X, [a, b, c]].
```
### Lists manipulation
Pattern matching allows you to extract elements from a list or split a list into its head and tail. eg:
```prolog
%%% list_predicate/3
list_predicate([H|T],H,T).

?- list_predicate([1,2,3],H,T). % true and H=1 and T=[2,3]
```
It is in fact possible to disassemble a list like that! what happens is that H (Head) is unified with the first list element, and T (Tail) unifiend with the remaining portion of list. It is also possible to create lists using the same predicate.
```prolog
%%% list_predicate/3
list_predicate([H|T],H,T).

?- list_predicate(List,1,[2,3]). % true and List = [1,2,3]
```
### Recursion
The #recursion is a fundamental concept in Prolog and plays a significant role in solving problems and performing tasks that involve repetitive or nested operations.
To work with recursion it is important to define 2 predicates:
1. **Base Case:** you start by defining a base case or termination condition. The base case specifies when the recursion should stop. Without a base case, the recursion will continue indefinitely, leading to a stack overflow.
2. **Recursive Case:** you define a recursive case that describes how to break down the problem into smaller subproblems and how to make a recursive call. The recursive call should be closer to the base case to ensure progress toward termination.
```prolog
%%% factorial/2
factorial(0, 1).           % Base case: factorial of 0 is 1.
factorial(N, Result):-     % Recursive case
	N > 0,
    N1 is N - 1,
    factorial(N1, SubResult),
    Result is N * SubResult.
```
### Recursive list processing
Prolog's recursive capabilities are well-suited for list processing. You can define predicates that operate on lists recursively, such as summing the elements of a list or searching for a specific element.
```prolog
iterate_list([]).         % Base case: empty list
iterate_list([H|T]):-     % Recursive case
    write(H),             % write to console Head
    nl,                   % go to newline
    iterate_list(T).      % recursive call

?- iterate_list([1,2,3]). % true and will write 1 2 3

iterate_list_2([]).       % Base case: empty list
iterate_list_2([H|T]):-   % Recursive case
    iterate_list_2(T),    % recursive call
    write(H),             % write to console Head
    nl.                   % go to newline

?- iterate_list([1,2,3]). % true and will write 3 2 1
```
Yes, predicates order matters.
### List operations and built-in predicates
Prolog provides built-in predicates for various list operations, including:

- `length/2`: Calculates the length of a list.
- `member/2`: Checks if an element is a member of a list.
- `append/3`: Concatenates two lists.
- `reverse/2`: Reverses the order of elements in a list.
- `nth0/3` and `nth1/3`: Access elements at specific positions in a list (0-based or 1-based indexing).

[[4 - Control Structures]]