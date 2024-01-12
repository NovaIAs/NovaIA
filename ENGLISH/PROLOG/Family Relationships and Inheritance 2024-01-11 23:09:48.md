```prolog
% Define the rules for the family relationships.

parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

father(X, Y) :- male(X), parent(X, Y).
mother(X, Y) :- female(X), parent(X, Y).

male(john).
male(bob).
male(tom).

female(mary).
female(alice).
female(sue).

% Define the rules for the inheritance of eye color.

eye_color(X, Y) :- parent(X, Y), eye_color(X, Z), eye_color(Y, Z).
eye_color(X, brown) :- male(X), parent(X, Y), eye_color(Y, brown).
eye_color(X, brown) :- female(X), parent(X, Y), eye_color(Y, brown), parent(X, Z), eye_color(Z, brown).
eye_color(X, blue) :- male(X), parent(X, Y), eye_color(Y, blue).
eye_color(X, blue) :- female(X), parent(X, Y), eye_color(Y, blue), parent(X, Z), eye_color(Z, blue).

% Define the rules for the inheritance of hair color.

hair_color(X, Y) :- parent(X, Y), hair_color(X, Z), hair_color(Y, Z).
hair_color(X, brown) :- male(X), parent(X, Y), hair_color(Y, brown).
hair_color(X, brown) :- female(X), parent(X, Y), hair_color(Y, brown), parent(X, Z), hair_color(Z, brown).
hair_color(X, blonde) :- male(X), parent(X, Y), hair_color(Y, blonde).
hair_color(X, blonde) :- female(X), parent(X, Y), hair_color(Y, blonde), parent(X, Z), hair_color(Z, blonde).

% Define the rules for the inheritance of height.

height(X, Y) :- parent(X, Y), height(X, Z), height(Y, Z).
height(X, tall) :- male(X), parent(X, Y), height(Y, tall).
height(X, tall) :- female(X), parent(X, Y), height(Y, tall), parent(X, Z), height(Z, tall).
height(X, short) :- male(X), parent(X, Y), height(Y, short).
height(X, short) :- female(X), parent(X, Y), height(Y, short), parent(X, Z), height(Z, short).

% Define the query to find all the descendants of John who have brown eyes and brown hair.

descendants_with_brown_eyes_and_brown_hair(X) :-
    parent(john, X),
    eye_color(X, brown),
    hair_color(X, brown).
```

This code defines a set of rules for representing family relationships, eye color, hair color, and height. It also includes a query to find all the descendants of John who have brown eyes and brown hair.

Here is a breakdown of the code:

* The first few lines define the rules for the family relationships. The `parent/2` predicate represents the parent-child relationship. The `father/2` and `mother/2` predicates represent the father-child and mother-child relationships, respectively. The `male/1` and `female/1` predicates represent the gender of a person.
* The next few lines define the rules for the inheritance of eye color. The `eye_color/2` predicate represents the eye color of a person. The rules specify that a person's eye color can be inherited from either parent or both parents.
* The next few lines define the rules for the inheritance of hair color. The `hair_color/2` predicate represents the hair color of a person. The rules specify that a person's hair color can be inherited from either parent or both parents.
* The next few lines define the rules for the inheritance of height. The `height/2` predicate represents the height of a person. The rules specify that a person's height can be inherited from either parent or both parents.
* The last few lines define the query to find all the descendants of John who have brown eyes and brown hair. The `descendants_with_brown_eyes_and_brown_hair/1` predicate represents the set of all such descendants.

To use this code, you would first need to load it into a Prolog interpreter. Then, you could use the `descendants_with_brown_eyes_and_brown_hair/1` predicate to find all the descendants of John who have brown eyes and brown hair.