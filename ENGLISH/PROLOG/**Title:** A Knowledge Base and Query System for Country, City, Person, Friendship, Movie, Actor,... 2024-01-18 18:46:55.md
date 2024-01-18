```prolog
:- dynamic country/3, city/3, person/4, friend/2, movie/3, actor/2, director/2.

% Countries
country(japan, 'Asia', 126_475_000).
country(canada, 'North America', 38_005_238).
country(brazil, 'South America', 212_559_409).
country(australia, 'Oceania', 25_768_727).
country(france, 'Europe', 65_273_511).

% Cities
city(tokyo, japan, 9_207_321).
city(ottawa, canada, 1_017_449).
city(saopaulo, brazil, 12_325_232).
city(sydney, australia, 5_312_434).
city(paris, france, 2_206_488).

% Persons
person(john, male, 30, tokyo).
person(mary, female, 25, ottawa).
person(bob, male, 40, saopaulo).
person(alice, female, 28, sydney).
person(tom, male, 35, paris).

% Friendships
friend(john, mary).
friend(mary, bob).
friend(bob, alice).
friend(alice, tom).
friend(tom, john).

% Movies
movie(starwars, 1977, george_lucas).
movie(empire_strikes_back, 1980, irvin_kershner).
movie(return_of_the_jedi, 1983, richard_marquand).
movie(phantom_menace, 1999, george_lucas).
movie(attack_of_the_clones, 2002, george_lucas).

% Actors
actor(mark_hamill, luke_skywalker).
actor(harrison_ford, han_solo).
actor(carrie_fisher, leia_organa).
actor(liam_neeson, qui_gon_jinn).
actor(ewan_mcgregor, obi_wan_kenobi).

% Directors
director(george_lucas, starwars).
director(george_lucas, phantom_menace).
director(george_lucas, attack_of_the_clones).
director(irvin_kershner, empire_strikes_back).
director(richard_marquand, return_of_the_jedi).

% Queries
?- country(brazil, Continent, Population).
?- city(X, Y, Population), Population > 5_000_000.
?- person(X, male, Age, tokyo), Age > 25.
?- friend(X, Y), friend(Y, Z), X \= Z.
?- movie(X, Year, Director), Year > 2000.
?- actor(X, Y), movie(M, _, Director), director(Director, M), actor(X, Z), Z \= Y.
?- director(X, M), actor(A, R), actor(A, S), movie(M, _, X), R \= S.
```

This code is a PROLOG program that contains information about countries, cities, people, friendships, movies, actors, and directors. It includes facts about each of these categories, as well as rules for querying the data.

The first part of the code defines the predicates that will be used to represent the information. The predicates are:

* `country/3`: Represents a country with three arguments: the country's name, the continent it is located in, and its population.
* `city/3`: Represents a city with three arguments: the city's name, the country it is located in, and its population.
* `person/4`: Represents a person with four arguments: the person's name, their gender, their age, and the city they live in.
* `friend/2`: Represents a friendship between two people.
* `movie/3`: Represents a movie with three arguments: the movie's name, the year it was released, and the director who directed it.
* `actor/2`: Represents an actor with two arguments: the actor's name and the role they played in a movie.
* `director/2`: Represents a director with two arguments: the director's name and the movie they directed.

The second part of the code contains facts about each of the categories. The facts are:

* Facts about countries:
    * Japan is located in Asia and has a population of 126,475,000.
    * Canada is located in North America and has a population of 38,005,238.
    * Brazil is located in South America and has a population of 212,559,409.
    * Australia is located in Oceania and has a population of 25,768,727.
    * France is located in Europe and has a population of 65,273,511.
* Facts about cities:
    * Tokyo is located in Japan and has a population of 9,207,321.
    * Ottawa is located in Canada and has a population of 1,017,449.
    * Sao Paulo is located in Brazil and has a population of 12,325,232.
    * Sydney is located in Australia and has a population of 5,312,434.
    * Paris is located in France and has a population of 2,206,488.
* Facts about people:
    * John is male, 30 years old, and lives in Tokyo.
    * Mary is female, 25 years old, and lives in Ottawa.
    * Bob is male, 40 years old, and lives in Sao Paulo.
    * Alice is female, 28 years old, and lives in Sydney.
    * Tom is male, 35 years old, and lives in Paris.
* Facts about friendships:
    * John is friends with Mary.
    * Mary is friends with Bob.
    * Bob is friends with Alice.
    * Alice is friends with Tom.
    * Tom is friends with John.
* Facts about movies:
    * Star Wars was released in 1977 and was directed by George Lucas.
    * The Empire Strikes Back was released in 1980 and was directed by Irvin Kershner.
    * Return of the Jedi was released in 1983 and was directed by Richard Marquand.
    * The Phantom Menace was released in 1999 and was directed by George Lucas.
    * Attack of the Clones was released in 2002 and was directed by George Lucas.
* Facts about actors:
    * Mark Hamill played Luke Skywalker.
    * Harrison Ford played Han Solo.
    * Carrie Fisher played Leia Organa.
    * Liam Neeson played Qui-Gon Jinn.
    * Ewan McGregor played Obi-Wan Kenobi.
* Facts about directors:
    * George Lucas directed Star Wars, The Phantom Menace, and Attack of the Clones.
    * Irvin Kershner directed The Empire Strikes Back.
    * Richard Marquand directed Return of the Jedi.

The third part of the code contains rules for querying the data. The rules are:

* `country(Country, Continent, Population)`: This rule finds the country with the given name, continent, and population.
* `city(City, Country, Population)`: This rule finds the city with the given name, country, and population.
* `person(Name, Gender, Age, City)`: This rule finds the person with the given name, gender, age, and city.
* `friend(X, Y)`: This rule finds two people who are friends.
* `movie(Movie, Year, Director)`: This rule finds the movie with the given name, year, and director.
* `actor(Actor, Role)`: This rule finds the actor who played the given role.
* `director(Director, Movie)`: This rule finds the director who directed the given movie.

The `?-` symbol at the beginning of each query is used to tell the PROLOG interpreter that the following line is a query. The `.` symbol at