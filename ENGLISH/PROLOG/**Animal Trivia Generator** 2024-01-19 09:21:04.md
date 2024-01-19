```
% Define the rule for deducing the number of legs of an animal based on its name.
numberOfLegs(Animal, 4) :-
    animal_with_four_legs(Animal),
    !.  % Cut operator prevents backtracking to other rules for this goal.

numberOfLegs(Animal, 2) :-
    animal_with_two_legs(Animal).

% Define a list of animals with four legs.
animal_with_four_legs(dog).
animal_with_four_legs(cat).
animal_with_four_legs(cow).
animal_with_four_legs(horse).
animal_with_four_legs(pig).

% Define a list of animals with two legs.
animal_with_two_legs(human).
animal_with_two_legs(chicken).
animal_with_two_legs(duck).
animal_with_two_legs(goose).

% Define the rule for deducing if an animal is a pet based on its name.
isPet(Animal, true) :-
    pet_animal(Animal),
    !.  % Cut operator prevents backtracking to other rules for this goal.

isPet(Animal, false).

% Define a list of pet animals.
pet_animal(dog).
pet_animal(cat).
pet_animal(fish).
pet_animal(bird).

% Define the rule for generating a random animal name.
randomAnimal(Animal) :-
    findall(Animal, (between(1, 10), animal_name(Animal)), AnimalList),
    random_member(Animal, AnimalList).

% Define a list of animal names.
animal_name(dog).
animal_name(cat).
animal_name(cow).
animal_name(horse).
animal_name(pig).
animal_name(human).
animal_name(chicken).
animal_name(duck).
animal_name(goose).
animal_name(fish).
animal_name(bird).

% Define the goal to generate a random animal name, its number of legs, and whether it is a pet.
generateRandomAnimalInfo :-
    randomAnimal(Animal),
    numberOfLegs(Animal, Legs),
    isPet(Animal, Pet),
    format('Animal: ~w, Legs: ~w, Pet: ~w~n', [Animal, Legs, Pet]).

% Call the goal to generate random animal information multiple times.
main :-
    forall(between(1, 10), generateRandomAnimalInfo).
```

Explanation:

1. We define rules to deduce the number of legs of an animal based on its name (`numberOfLegs/2`) and to check if an animal is a pet based on its name (`isPet/2`).

2. We define lists of animals with four legs (`animal_with_four_legs/1`) and two legs (`animal_with_two_legs/1`).

3. We define a rule to generate a random animal name (`randomAnimal/1`).

4. We define a list of animal names (`animal_name/1`).

5. We define a goal (`generateRandomAnimalInfo`) to generate random animal information, including the animal's name, number of legs, and whether it is a pet.

6. We call the `main` goal to generate random animal information multiple times (10 times in this case).

This code demonstrates complex rule definitions, list processing, and random number generation in Prolog, resulting in a program that generates diverse animal information in a structured manner.