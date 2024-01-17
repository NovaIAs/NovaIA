```groovy
import groovy.transform.CompileStatic
import groovy.util.logging.Slf4j

@CompileStatic
@Slf4j
class ComplexGroovyCode {

    private static final String[] NAMES = ["John", "Mary", "Bob", "Alice", "Tom"]
    private static final List<Integer> NUMBERS = [1, 2, 3, 4, 5]

    static void main(String[] args) {
        // Create a map of names to numbers
        Map<String, Integer> nameToNumberMap = NAMES.zip(NUMBERS).toMap()

        // Print the map
        log.info("Name to number map: $nameToNumberMap")

        // Create a list of tuples containing names and numbers
        List<Tuple2<String, Integer>> nameNumberTuples = NAMES.zip(NUMBERS)

        // Print the list of tuples
        log.info("List of name-number tuples: $nameNumberTuples")

        // Create a closure that takes a name and returns the corresponding number
        Closure<Integer> nameToNumberClosure = { String name -> nameToNumberMap[name] }

        // Print the result of applying the closure to each name in the NAMES array
        log.info("Numbers corresponding to names: ${NAMES.collect(nameToNumberClosure)}")

        // Create a method that takes a list of numbers and returns the sum
        def sumNumbers = { List<Integer> numbers -> numbers.sum() }

        // Print the sum of the NUMBERS list
        log.info("Sum of numbers: ${sumNumbers(NUMBERS)}")

        // Create a class that represents a person
        class Person {
            String name
            int age

            Person(String name, int age) {
                this.name = name
                this.age = age
            }

            String toString() {
                "Person{name='$name', age=$age}"
            }
        }

        // Create a list of people
        List<Person> people = [
                new Person("John", 25),
                new Person("Mary", 30),
                new Person("Bob", 35),
                new Person("Alice", 40),
                new Person("Tom", 45)
        ]

        // Print the list of people
        log.info("List of people: $people")

        // Create a closure that takes a person and returns their age
        Closure<Integer> getAgeClosure = { Person person -> person.age }

        // Print the ages of the people in the people list
        log.info("Ages of the people: ${people.collect(getAgeClosure)}")

        // Create a method that takes a list of people and returns the oldest person
        def getOldestPerson = { List<Person> people -> people.max(getAgeClosure) }

        // Print the oldest person in the people list
        log.info("Oldest person: ${getOldestPerson(people)}")
    }
}
```

Explanation:

This Groovy script demonstrates various complex and differentiated features of the language. It includes:

1. **Maps and Tuples**: The script creates a map of names to numbers using the `zip()` method and converts it to a map using `toMap()`. It also creates a list of tuples containing names and numbers using `zip()`.

2. **Closures**: The script defines a closure that takes a name and returns the corresponding number from the map. It also defines a method that takes a list of numbers and returns the sum.

3. **Classes and Objects**: The script defines a `Person` class with properties `name` and `age`, and a `toString()` method to represent the person as a string. It then creates a list of `Person` objects.

4. **Method References**: The script uses method references (`getAgeClosure`) to reference the `getAge()` method of the `Person` class. This allows for more concise and readable code.

5. **Collection Operations**: The script uses Groovy's powerful collection operations, such as `collect()`, `max()`, and `sum()`, to manipulate and analyze the lists of names, numbers, and people.

Overall, this script showcases the versatility and expressiveness of Groovy, making it a suitable language for a wide range of programming tasks.