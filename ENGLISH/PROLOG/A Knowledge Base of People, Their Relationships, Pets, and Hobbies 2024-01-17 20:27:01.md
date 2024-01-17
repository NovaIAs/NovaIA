```prolog
% Define the family relationships
father(john, mary).
father(john, bob).
mother(mary, mary).
mother(mary, bob).

% Define the gender of each person
male(john).
male(bob).
female(mary).

% Define the age of each person
age(john, 50).
age(mary, 45).
age(bob, 15).

% Define the occupation of each person
occupation(john, doctor).
occupation(mary, teacher).
occupation(bob, student).

% Define the city where each person lives
city(john, london).
city(mary, london).
city(bob, london).

% Define the hobbies of each person
hobby(john, golf).
hobby(mary, reading).
hobby(bob, playing video games).

% Define the relationship between each person and their pet
pet(john, dog).
pet(mary, cat).
pet(bob, fish).

% Define the name of each person's pet
name(john_dog, buddy).
name(mary_cat, kitty).
name(bob_fish, nemo).

% Define the breed of each person's pet
breed(john_dog, golden retriever).
breed(mary_cat, persian).
breed(bob_fish, goldfish).

% Define the age of each person's pet
age(john_dog, 10).
age(mary_cat, 5).
age(bob_fish, 2).

% Define the rules for finding information about a person

% Find the name of the person with the specified age
person_with_age(Age, Name) :-
  age(Name, Age).

% Find the occupation of the person with the specified name
occupation_of_person(Name, Occupation) :-
  occupation(Name, Occupation).

% Find the city where the person with the specified name lives
city_of_person(Name, City) :-
  city(Name, City).

% Find the hobbies of the person with the specified name
hobbies_of_person(Name, Hobbies) :-
  findall(Hobby, hobby(Name, Hobby), Hobbies).

% Find the relationship between the person with the specified name and their pet
relationship_with_pet(Name, Relationship) :-
  pet(Name, Pet),
  relationship(Pet, Relationship).

% Find the name of the pet of the person with the specified name
name_of_pet(Name, PetName) :-
  pet(Name, Pet),
  name(Pet, PetName).

% Find the breed of the pet of the person with the specified name
breed_of_pet(Name, Breed) :-
  pet(Name, Pet),
  breed(Pet, Breed).

% Find the age of the pet of the person with the specified name
age_of_pet(Name, Age) :-
  pet(Name, Pet),
  age(Pet, Age).

% Find the family members of the person with the specified name
family_members(Name, FamilyMembers) :-
  setof(FamilyMember, (father(Name, FamilyMember); mother(Name, FamilyMember)), FamilyMembers).

% Find the friends of the person with the specified name
friends(Name, Friends) :-
  setof(Friend, (friend_of(Name, Friend); friend_of(Friend, Name)), Friends).

% Find the people who live in the same city as the person with the specified name
people_in_same_city(Name, People) :-
  city(Name, City),
  setof(Person, city(Person, City), People).

% Find the people who have the same hobby as the person with the specified name
people_with_same_hobby(Name, People) :-
  hobby(Name, Hobby),
  setof(Person, hobby(Person, Hobby), People).
```

This code defines a complex knowledge base about people, their relationships, their pets, and their hobbies. It also defines rules for finding information about a person based on their name, age, occupation, city, hobbies, pet, and family members.

The code can be used to answer various questions about the people in the knowledge base. For example, you could ask:

* What is the name of the person who is 50 years old?
* What is the occupation of the person named John?
* Where does the person named Mary live?
* What are the hobbies of the person named Bob?
* What is the relationship between the person named John and his pet?
* What is the name of the pet of the person named Mary?
* What is the breed of the pet of the person named Bob?
* What is the age of the pet of the person named John?
* Who are the family members of the person named John?
* Who are the friends of the person named Mary?
* Who are the people who live in the same city as the person named Bob?
* Who are the people who have the same hobby as the person named John?

To use the code, you would first need to load it into a Prolog interpreter. Then, you could use the various predicates defined in the code to answer your questions. For example, to find the name of the person who is 50 years old, you would use the following query:

```prolog
?- person_with_age(50, Name).
```

This query would return the name of the person who is 50 years old, which is John.