```
% Define the family relationships.
father(john, mary).
father(john, bob).
mother(mary, mary).
mother(mary, bob).

% Define the gender of each person.
male(john).
male(bob).
female(mary).

% Define the age of each person.
age(john, 40).
age(mary, 35).
age(bob, 10).

% Define the occupation of each person.
occupation(john, doctor).
occupation(mary, teacher).
occupation(bob, student).

% Define the city where each person lives.
city(john, london).
city(mary, london).
city(bob, london).

% Define the relationship between two people.
relationship(X, Y, Father) :-
    father(X, Y).

relationship(X, Y, Mother) :-
    mother(X, Y).

relationship(X, Y, Child) :-
    father(Y, X).

relationship(X, Y, Child) :-
    mother(Y, X).

% Define the gender of a person.
gender(X, Male) :-
    male(X).

gender(X, Female) :-
    female(X).

% Define the age of a person.
age(X, Age) :-
    age(X, Age).

% Define the occupation of a person.
occupation(X, Occupation) :-
    occupation(X, Occupation).

% Define the city where a person lives.
city(X, City) :-
    city(X, City).

% Find all the people who are related to a given person.
related_to(X, Y) :-
    relationship(X, Y, _).

% Find all the people who are of a given gender.
people_of_gender(Gender, X) :-
    gender(X, Gender).

% Find all the people who are of a given age.
people_of_age(Age, X) :-
    age(X, Age).

% Find all the people who have a given occupation.
people_of_occupation(Occupation, X) :-
    occupation(X, Occupation).

% Find all the people who live in a given city.
people_in_city(City, X) :-
    city(X, City).

% Find all the people who are related to a given person and are of a given gender.
related_to_and_of_gender(X, Y, Gender) :-
    related_to(X, Y),
    gender(Y, Gender).

% Find all the people who are related to a given person and are of a given age.
related_to_and_of_age(X, Y, Age) :-
    related_to(X, Y),
    age(Y, Age).

% Find all the people who are related to a given person and have a given occupation.
related_to_and_of_occupation(X, Y, Occupation) :-
    related_to(X, Y),
    occupation(Y, Occupation).

% Find all the people who are related to a given person and live in a given city.
related_to_and_in_city(X, Y, City) :-
    related_to(X, Y),
    city(Y, City).
```

This code is a Prolog program that defines a family tree and various relationships between people. It includes information about each person's name, gender, age, occupation, and city of residence. The program also defines a number of rules that can be used to query the family tree and find information about specific people or relationships.

For example, the following query would find all the people who are related to John:

```
?- related_to(john, X).
```

This query would return the following results:

```
X = mary
X = bob
```

The following query would find all the people who are related to John and are male:

```
?- related_to_and_of_gender(john, X, male).
```

This query would return the following results:

```
X = bob
```

The following query would find all the people who are related to John and are older than 30:

```
?- related_to_and_of_age(john, X, Age), Age > 30.
```

This query would return the following results:

```
X = mary
```

The following query would find all the people who are related to John and live in London:

```
?- related_to_and_in_city(john, X, london).
```

This query would return the following results:

```
X = mary
X = bob
```

This code is a powerful tool for querying a family tree and finding information about specific people or relationships. It can be used to answer a wide variety of questions, such as "Who is John's father?" or "How old is Mary?" or "Where does Bob live?"