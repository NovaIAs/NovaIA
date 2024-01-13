```groovy
// Define a class to represent a person
class Person {
    String name
    int age
    String occupation

    // Constructor to initialize the object
    Person(String name, int age, String occupation) {
        this.name = name
        this.age = age
        this.occupation = occupation
    }

    // Method to print the person's information
    void printInfo() {
        println "Name: $name"
        println "Age: $age"
        println "Occupation: $occupation"
    }
}

// Define a list of people
List<Person> people = [
    new Person("John Doe", 25, "Software Engineer"),
    new Person("Jane Smith", 30, "Doctor"),
    new Person("Michael Jones", 35, "Teacher"),
    new Person("Sarah Miller", 40, "Lawyer"),
    new Person("David Brown", 45, "Businessman")
]

// Print the information of each person in the list
for (Person person in people) {
    person.printInfo()
    println()
}

// Find the oldest person in the list
Person oldestPerson = people.max { it.age }

// Print the information of the oldest person
println "Oldest Person:"
oldestPerson.printInfo()

// Find the youngest person in the list
Person youngestPerson = people.min { it.age }

// Print the information of the youngest person
println "Youngest Person:"
youngestPerson.printInfo()

// Group the people by their occupation
Map<String, List<Person>> peopleByOccupation = people.groupBy { it.occupation }

// Print the occupations and the people associated with each occupation
println "People by Occupation:"
peopleByOccupation.each { occupation, people ->
    println "Occupation: $occupation"
    people.each { person ->
        println "  $person.name"
    }
    println()
}

// Sort the people by their name
List<Person> sortedPeople = people.sort { it.name }

// Print the information of the sorted people
println "Sorted People:"
sortedPeople.each { person ->
    person.printInfo()
    println()
}
```

This code defines a class called `Person` to represent a person with attributes such as name, age, and occupation. It then creates a list of `Person` objects and performs various operations on the list, such as printing the information of each person, finding the oldest and youngest persons, grouping the people by their occupation, and sorting the people by their name. The code also includes comments to explain what each part of the code does.

Here is a breakdown of the code:

1. **Class Definition:**

   ```groovy
   class Person {
       String name
       int age
       String occupation

       // Constructor to initialize the object
       Person(String name, int age, String occupation) {
           this.name = name
           this.age = age
           this.occupation = occupation
       }

       // Method to print the person's information
       void printInfo() {
           println "Name: $name"
           println "Age: $age"
           println "Occupation: $occupation"
       }
   }
   ```

   This defines a class called `Person` with three attributes: `name`, `age`, and `occupation`. It also includes a constructor to initialize the object and a method called `printInfo()` to print the person's information.

2. **Creating a List of People:**

   ```groovy
   List<Person> people = [
       new Person("John Doe", 25, "Software Engineer"),
       new Person("Jane Smith", 30, "Doctor"),
       new Person("Michael Jones", 35, "Teacher"),
       new Person("Sarah Miller", 40, "Lawyer"),
       new Person("David Brown", 45, "Businessman")
   ]
   ```

   This creates a list of `Person` objects, each representing a different person with their name, age, and occupation.

3. **Printing the Information of Each Person:**

   ```groovy
   for (Person person in people) {
       person.printInfo()
       println()
   }
   ```

   This iterates over the list of people and calls the `printInfo()` method on each person to print their information.

4. **Finding the Oldest and Youngest Persons:**

   ```groovy
   Person oldestPerson = people.max { it.age }
   Person youngestPerson = people.min { it.age }
   ```

   These lines find the oldest and youngest persons in the list using the `max()` and `min()` methods, respectively. The `max()` and `min()` methods take a closure as an argument, which specifies how to compare the elements of the list. In this case, the closure is `it.age`, which compares the age of each person.

5. **Grouping the People by Their Occupation:**

   ```groovy
   Map<String, List<Person>> peopleByOccupation = people.groupBy { it.occupation }
   ```

   This line groups the people in the list by their occupation using the `groupBy()` method. The `groupBy()` method takes a closure as an argument, which specifies how to group the elements of the list. In this case, the closure is `it.occupation`, which groups the people by their occupation.

6. **Printing the Occupations and the People Associated with Each Occupation:**

   ```groovy
   println "People by Occupation:"
   peopleByOccupation.each { occupation, people ->
       println "Occupation: $occupation"
       people.each { person ->
           println "  $person.name"
       }
       println()
   }
   ```

   This iterates over the map of occupations and people and prints the occupation along with the names of the people associated with that occupation.

7. **Sorting the People by Their Name:**

   ```groovy
   List<Person> sortedPeople = people.sort { it.name }
   ```

   This line sorts the list of people by their name using the `sort()` method. The `sort()` method takes a closure as an argument, which specifies how to compare the elements of the list. In this case, the closure is `it.name`, which compares the names of the people.

8. **Printing the Information of the Sorted People:**

   ```groovy
   println "Sorted People:"
   sortedPeople.each { person ->
       person.printInfo()
       println()
   }
   ```

   This iterates over the sorted list of people and calls the `printInfo()` method on each person to print their information.