```prolog
% Define the family relationships
parent(john, mary).
parent(john, bob).
parent(mary, alice).
parent(bob, charlie).
parent(alice, eve).

% Define the gender of each person
male(john).
male(bob).
male(charlie).
female(mary).
female(alice).
female(eve).

% Define the rules for determining if someone is a grandparent
grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

% Define the rules for determining if someone is a sibling
sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

% Define the rules for determining if someone is an aunt or uncle
aunt_or_uncle(X, Y) :-
    sibling(X, Z),
    parent(Z, Y),
    X \= Z.

% Define the rules for determining if someone is a cousin
cousin(X, Y) :-
    aunt_or_uncle(Z, X),
    parent(Z, Y),
    X \= Y.

% Define the rules for determining if someone is a nephew or niece
nephew_or_niece(X, Y) :-
    sibling(Z, Y),
    parent(Z, X),
    X \= Y.

% Define the rules for determining if someone is a grandnephew or grandniece
grandnephew_or_grandniece(X, Y) :-
    nephew_or_niece(Z, Y),
    parent(Z, X),
    X \= Y.
```

Explanation:

1. **Defining Family Relationships:**
   - `parent(X, Y)`: This rule defines that `X` is the parent of `Y`.
   - `male(X)` and `female(X)`: These rules specify the gender of each person.

2. **Determining Grandparent:**
   - `grandparent(X, Y)`: This rule states that `X` is the grandparent of `Y` if `X` is the parent of `Z`, and `Z` is the parent of `Y`.

3. **Determining Sibling:**
   - `sibling(X, Y)`: This rule defines that `X` and `Y` are siblings if they have the same parents.

4. **Determining Aunt or Uncle:**
   - `aunt_or_uncle(X, Y)`: This rule defines that `X` is the aunt or uncle of `Y` if `X` is the sibling of `Z`, and `Z` is the parent of `Y`.

5. **Determining Cousin:**
   - `cousin(X, Y)`: This rule specifies that `X` is the cousin of `Y` if `X`'s aunt or uncle is the parent of `Y`.

6. **Determining Nephew or Niece:**
   - `nephew_or_niece(X, Y)`: This rule defines that `X` is the nephew or niece of `Y` if `X`'s sibling is the parent of `Y`.

7. **Determining Grandnephew or Grandniece:**
   - `grandnephew_or_grandniece(X, Y)`: This rule specifies that `X` is the grandnephew or grandniece of `Y` if `X`'s nephew or niece is the parent of `Y`.

This code provides a comprehensive way to determine various family relationships based on the given information about parents and gender. It covers grandparent, sibling, aunt/uncle, cousin, nephew/niece, and grandnephew/grandniece relationships.