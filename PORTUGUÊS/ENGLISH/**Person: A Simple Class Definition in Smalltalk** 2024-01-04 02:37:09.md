```smalltalk
Object subclass: Person [
    classVariableNames: 'allPeople';
    instanceVariableNames: 'name age';
    classMethods: [
        allPeople -> ^ (allPeople := Set new)
        newFromName: aName anAge: anAge -> ^ self new setValues: { name: aName; age: anAge }
        findByName: aName -> ^ allPeople find: [:person | person name = aName]
    ];
    instanceMethods: [
        name -> ^ name
        age -> ^ age
        setName: aName -> name := aName
        setAge: anAge -> age := anAge
        isAdult -> ^ age >= 18
    ]
];

Person allPeople new;
Person newFromName: 'John Doe' anAge: 30;
Person newFromName: 'Jane Doe' anAge: 25;
(Person findByName: 'John Doe') isAdult; "true"
(Person findByName: 'Jane Doe') isAdult; "false"
```

Explanation:

* The code defines a class called `Person` that represents a person with a name and an age.
* The class has three class methods:
    * `allPeople` returns a set of all the people created so far.
    * `newFromName: anAge:` creates a new person with the given name and age and adds it to the set of all people.
    * `findByName:` finds a person in the set of all people with the given name and returns it.
* The class has four instance methods:
    * `name` returns the name of the person.
    * `age` returns the age of the person.
    * `setName:` sets the name of the person.
    * `setAge:` sets the age of the person.
    * `isAdult` returns true if the person is 18 or older, and false otherwise.
* The code creates a set of all people, creates two people named "John Doe" and "Jane Doe", and then checks if they are adults.