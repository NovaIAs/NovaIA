```prolog
% Define a knowledge base of facts and rules about animals.

% Facts about animals.
animal(dog).
animal(cat).
animal(bird).
animal(fish).
animal(horse).
animal(cow).
animal(pig).
animal(sheep).
animal(goat).
animal(chicken).
animal(duck).
animal(goose).

% Rules about animals.
has_fur(X) :- animal(X), X = dog.
has_fur(X) :- animal(X), X = cat.
has_feathers(X) :- animal(X), X = bird.
has_scales(X) :- animal(X), X = fish.
has_hooves(X) :- animal(X), X = horse.
has_hooves(X) :- animal(X), X = cow.
has_hooves(X) :- animal(X), X = pig.
has_hooves(X) :- animal(X), X = sheep.
has_hooves(X) :- animal(X), X = goat.
can_fly(X) :- animal(X), X = bird.
can_swim(X) :- animal(X), X = fish.
can_run(X) :- animal(X), X = horse.
can_run(X) :- animal(X), X = cow.
can_run(X) :- animal(X), X = pig.
can_run(X) :- animal(X), X = sheep.
can_run(X) :- animal(X), X = goat.
can_lay_eggs(X) :- animal(X), X = bird.
can_lay_eggs(X) :- animal(X), X = chicken.
can_lay_eggs(X) :- animal(X), X = duck.
can_lay_eggs(X) :- animal(X), X = goose.

% Define a query to find all animals that have fur.

?- animal(X), has_fur(X).

% Expected output:

% X = dog
% X = cat
```

Explanation:

* The first part of the code defines a knowledge base of facts and rules about animals. The facts are stored as Prolog clauses with the format `animal(X)`, where `X` is the name of the animal. The rules are stored as Prolog clauses with the format `has_fur(X) :- animal(X), X = dog`, where `has_fur(X)` is the goal to be proved, `animal(X)` is a condition that must be satisfied, and `X = dog` is a fact that must be true.
* The second part of the code defines a query to find all animals that have fur. The query is written as `?- animal(X), has_fur(X)`. This query asks Prolog to find all values of `X` such that both `animal(X)` and `has_fur(X)` are true.
* When the query is executed, Prolog will search through the knowledge base and find all the animals that have fur. In this case, the expected output is:

```
X = dog
X = cat
```

This means that the only animals in the knowledge base that have fur are dogs and cats.