```groovy
    // Define a Groovy class to represent a Person object
    class Person {
        // Fields to represent the person's name and age
        String name
        int age

        // Constructor to initialize the name and age fields
        Person(String name, int age) {
            this.name = name
            this.age = age
        }

        // Method to return the person's name
        String getName() {
            return name
        }

        // Method to return the person's age
        int getAge() {
            return age
        }

        // Override the default toString() method to provide a custom representation of the Person object
        @Override
        String toString() {
            return "Person{name='$name', age=$age}"
        }
    }

    // Define a Groovy class to represent a List of Person objects
    class PersonList {
        // Field to store the list of Person objects
        List<Person> people = []

        // Method to add a Person object to the list
        void addPerson(Person person) {
            people << person
        }

        // Method to remove a Person object from the list
        void removePerson(Person person) {
            people.remove(person)
        }

        // Method to get the number of Person objects in the list
        int size() {
            return people.size()
        }

        // Method to get a Person object from the list by its index
        Person getPerson(int index) {
            return people[index]
        }

        // Override the default toString() method to provide a custom representation of the PersonList object
        @Override
        String toString() {
            return "PersonList{people=$people}"
        }
    }

    // Create a PersonList object
    PersonList personList = new PersonList()

    // Add some Person objects to the list
    personList.addPerson(new Person("John Doe", 25))
    personList.addPerson(new Person("Jane Smith", 30))
    personList.addPerson(new Person("Michael Jones", 35))

    // Print the PersonList object to the console
    println personList

    // Get the first Person object from the list
    Person firstPerson = personList.getPerson(0)

    // Print the first person's name to the console
    println firstPerson.name

    // Remove the first person from the list
    personList.removePerson(firstPerson)

    // Print the PersonList object to the console again
    println personList
```

Explanation:

1. We define a Groovy class called `Person` to represent a person object. It has fields for the person's name and age, a constructor to initialize the fields, methods to get the person's name and age, and an overridden `toString()` method to provide a custom representation of the `Person` object.


2. We define another Groovy class called `PersonList` to represent a list of `Person` objects. It has a field to store the list of people, methods to add and remove people from the list, a method to get the number of people in the list, a method to get a person from the list by its index, and an overridden `toString()` method to provide a custom representation of the `PersonList` object.


3. We create an instance of the `PersonList` class and add some `Person` objects to it.


4. We print the `PersonList` object to the console to see the list of people.


5. We get the first person from the list and print their name to the console.


6. We remove the first person from the list and print the `PersonList` object to the console again to see the updated list.