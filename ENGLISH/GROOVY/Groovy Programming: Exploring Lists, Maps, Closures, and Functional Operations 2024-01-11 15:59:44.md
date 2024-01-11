```groovy
class GroovyCode {

    static void main(String[] args) {

        // Define a list of names
        List<String> names = ["John", "Mary", "Bob", "Alice", "Tom"]

        // Define a map of ages
        Map<String, Integer> ages = [
            "John": 25,
            "Mary": 30,
            "Bob": 35,
            "Alice": 40,
            "Tom": 45
        ]

        // Define a closure to print the name and age of a person
        Closure printPerson = { String name, Integer age ->
            println "$name is $age years old."
        }

        // Iterate over the list of names and print the name and age of each person
        names.each { String name ->
            printPerson(name, ages[name])
        }

        // Define a closure to calculate the sum of a list of numbers
        Closure sum = { List<Integer> numbers ->
            Integer result = 0
            for (Integer number : numbers) {
                result += number
            }
            return result
        }

        // Define a list of numbers
        List<Integer> numbers = [1, 2, 3, 4, 5]

        // Calculate the sum of the list of numbers using the closure
        Integer total = sum(numbers)

        // Print the total
        println "The sum of the numbers is $total."

        // Define a closure to find the maximum value in a list of numbers
        Closure max = { List<Integer> numbers ->
            Integer maxValue = numbers[0]
            for (Integer number : numbers) {
                if (number > maxValue) {
                    maxValue = number
                }
            }
            return maxValue
        }

        // Find the maximum value in the list of numbers using the closure
        Integer maximum = max(numbers)

        // Print the maximum value
        println "The maximum value in the list is $maximum."

        // Define a closure to create a new list of numbers by filtering the original list
        Closure filter = { List<Integer> numbers, Closure predicate ->
            List<Integer> filteredNumbers = []
            for (Integer number : numbers) {
                if (predicate(number)) {
                    filteredNumbers.add(number)
                }
            }
            return filteredNumbers
        }

        // Create a new list of numbers by filtering the original list to only include even numbers
        List<Integer> evenNumbers = filter(numbers, { Integer number -> number % 2 == 0 })

        // Print the filtered list of even numbers
        println "The even numbers in the list are $evenNumbers."

        // Define a closure to create a new list of numbers by mapping the original list
        Closure map = { List<Integer> numbers, Closure transformation ->
            List<Integer> mappedNumbers = []
            for (Integer number : numbers) {
                mappedNumbers.add(transformation(number))
            }
            return mappedNumbers
        }

        // Create a new list of numbers by mapping the original list to double each number
        List<Integer> doubledNumbers = map(numbers, { Integer number -> number * 2 })

        // Print the mapped list of doubled numbers
        println "The doubled numbers in the list are $doubledNumbers."
    }
}
```

Explanation:

1. **Define Data Structures**:
   - `List<String> names`: A list of names.
   - `Map<String, Integer> ages`: A map of names to ages.

2. **Define Closure for Printing Person**:
   - `Closure printPerson`: A closure that takes a name and age and prints them.

3. **Iterate Over Names and Print Person**:
   - Iterate over the list of names and for each name, use the `printPerson` closure to print the name and age.

4. **Define Closure for Summing Numbers**:
   - `Closure sum`: A closure that takes a list of numbers and returns their sum.

5. **Calculate Sum of Numbers**:
   - Use the `sum` closure to calculate the sum of a list of numbers.

6. **Define Closure for Finding Maximum Value**:
   - `Closure max`: A closure that takes a list of numbers and returns the maximum value.

7. **Find Maximum Value in Numbers**:
   - Use the `max` closure to find the maximum value in a list of numbers.

8. **Define Closure for Filtering Numbers**:
   - `Closure filter`: A closure that takes a list of numbers and a predicate closure. It returns a new list of numbers that satisfy the predicate.

9. **Filter Even Numbers**:
   - Use the `filter` closure to filter the list of numbers to only include even numbers.

10. **Define Closure for Mapping Numbers**:
    - `Closure map`: A closure that takes a list of numbers and a transformation closure. It returns a new list of numbers by applying the transformation to each number in the original list.

11. **Map Numbers to Doubled Values**:
    - Use the `map` closure to create a new list of numbers by doubling each number in the original list.