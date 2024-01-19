```groovy
import groovy.transform.ToString

@ToString
class Person {
    String firstName
    String lastName
    int age
    List<String> hobbies
}

Person alice = new Person(
        firstName: "Alice",
        lastName: "Smith",
        age: 25,
        hobbies: ["running", "reading", "cooking"]
)

Person bob = new Person(
        firstName: "Bob",
        lastName: "Johnson",
        age: 30,
        hobbies: ["hiking", "fishing", "camping"]
)

List<Person> people = [alice, bob]

people.each { person ->
    println "Name: ${person.firstName} ${person.lastName}"
    println "Age: ${person.age}"
    println "Hobbies:"
    person.hobbies.each { hobby ->
        println "\t${hobby}"
    }
    println ""
}
```

This is a Groovy script that defines a `Person` class, creates two `Person` objects, and then prints information about each person.

The `Person` class has four properties: `firstName`, `lastName`, `age`, and `hobbies`. The `firstName` and `lastName` properties are strings, the `age` property is an integer, and the `hobbies` property is a list of strings.

The `Person` class also has a `toString()` method, which is used to generate a string representation of a `Person` object. The `toString()` method is invoked by the `println` statement in the script.

The `people` variable is a list of `Person` objects. The `each` method is used to iterate over the `people` list and print information about each person.

The `firstName`, `lastName`, and `age` properties of each `Person` object are printed using the `println` statement. The `hobbies` property is printed using a nested `each` loop.

The output of the script is:

```
Name: Alice Smith
Age: 25
Hobbies:
        running
        reading
        cooking

Name: Bob Johnson
Age: 30
Hobbies:
        hiking
        fishing
        camping
```