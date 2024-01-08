```prolog
% Define a knowledge base of facts and rules about family relationships.

father(john, bob).
father(john, alice).
mother(mary, bob).
mother(mary, alice).
male(john).
male(bob).
female(mary).
female(alice).

% Define a rule to infer the gender of a person based on their parents' genders.

gender(Person, Gender) :-
    father(Father, Person),
    male(Father),
    Gender = male.
gender(Person, Gender) :-
    mother(Mother, Person),
    female(Mother),
    Gender = female.

% Define a rule to infer the relationship between two people based on their parents.

relationship(Person1, Person2, Relationship) :-
    father(Person1, Person2),
    Relationship = father.
relationship(Person1, Person2, Relationship) :-
    mother(Person1, Person2),
    Relationship = mother.
relationship(Person1, Person2, Relationship) :-
    father(Parent1, Person1),
    father(Parent2, Person2),
    Parent1 = Parent2,
    Relationship = brother.
relationship(Person1, Person2, Relationship) :-
    mother(Parent1, Person1),
    mother(Parent2, Person2),
    Parent1 = Parent2,
    Relationship = sister.
relationship(Person1, Person2, Relationship) :-
    father(Parent1, Person1),
    mother(Parent2, Person2),
    Parent1 = Parent2,
    Relationship = half-brother.
relationship(Person1, Person2, Relationship) :-
    mother(Parent1, Person1),
    father(Parent2, Person2),
    Parent1 = Parent2,
    Relationship = half-sister.

% Define a rule to infer the ancestors of a person.

ancestors(Person, Ancestor) :-
    father(Ancestor, Person).
ancestors(Person, Ancestor) :-
    mother(Ancestor, Person).
ancestors(Person, Ancestor) :-
    father(Parent, Person),
    ancestors(Parent, Ancestor).
ancestors(Person, Ancestor) :-
    mother(Parent, Person),
    ancestors(Parent, Ancestor).

% Define a rule to infer the descendants of a person.

descendants(Person, Descendant) :-
    father(Person, Descendant).
descendants(Person, Descendant) :-
    mother(Person, Descendant).
descendants(Person, Descendant) :-
    father(Parent, Descendant),
    descendants(Person, Parent).
descendants(Person, Descendant) :-
    mother(Parent, Descendant),
    descendants(Person, Parent).

% Define a rule to infer the siblings of a person.

siblings(Person1, Person2) :-
    father(Parent, Person1),
    father(Parent, Person2),
    Person1 \= Person2.
siblings(Person1, Person2) :-
    mother(Parent, Person1),
    mother(Parent, Person2),
    Person1 \= Person2.

% Define a rule to infer the cousins of a person.

cousins(Person1, Person2) :-
    ancestors(Person1, Ancestor1),
    ancestors(Person2, Ancestor2),
    Ancestor1 = Ancestor2,
    Person1 \= Person2.

% Define a query to find all the ancestors of a person.

?- ancestors(bob, Ancestor).

% Define a query to find all the descendants of a person.

?- descendants(john, Descendant).

% Define a query to find all the siblings of a person.

?- siblings(bob, Sibling).

% Define a query to find all the cousins of a person.

?- cousins(bob, Cousin).
```

This Prolog code defines a knowledge base of facts and rules about family relationships. It includes facts about who is the father and mother of each person, as well as the gender of each person. The rules allow us to infer the gender of a person based on their parents' genders, the relationship between two people based on their parents, the ancestors of a person, the descendants of a person, the siblings of a person, and the cousins of a person. The queries at the end of the code demonstrate how to use the knowledge base to answer questions about family relationships.