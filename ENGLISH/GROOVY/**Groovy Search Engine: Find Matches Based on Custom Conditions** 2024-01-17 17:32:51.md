```groovy
class Search {
    static <T> List<T> findMatches(List<T> list, Predicate<T> condition) {
        def results = new ArrayList<T>()
        for (T item : list) {
            if (condition.test(item)) {
                results.add(item)
            }
        }
        return results
    }
}

def numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

def evenNumbers = Search.findMatches(numbers, { it % 2 == 0 })
println evenNumbers

def oddNumbers = Search.findMatches(numbers, { it % 2 != 0 })
println oddNumbers

def primeNumbers = Search.findMatches(numbers, {
    if(it == 1) {
        return false
    }
    boolean isPrime = true
    for(int i = 2; i < it/2 + 1; i++) {
        if(it % i == 0) {
            isPrime = false
        }
    }
    return isPrime
})
println primeNumbers
```

Explanation:

1. `class Search { ... }`: This defines a class named `Search` that contains a static method named `findMatches`.

2. `static <T> List<T> findMatches(List<T> list, Predicate<T> condition)`: This defines a static method named `findMatches` that takes two parameters:

   - `List<T> list`: The list to be searched.
   - `Predicate<T> condition`: A predicate that determines whether an item in the list matches the search criteria.

3. `def results = new ArrayList<T>()`: Creates an empty ArrayList to store the matching items.

4. `for (T item : list) { ... }`: This loop iterates over each item in the list and checks if it satisfies the condition using the `condition.test(item)` call.

5. `if (condition.test(item)) { ... }`: If the condition is satisfied, the item is added to the `results` list.

6. `return results`: The method returns the list of matching items.

7. `def numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`: This line creates a list of numbers from 1 to 10.

8. `def evenNumbers = Search.findMatches(numbers, { it % 2 == 0 })`: This line uses the `findMatches` method to search for even numbers in the `numbers` list.

9. `println evenNumbers`: This line prints the list of even numbers to the console.

10. `def oddNumbers = Search.findMatches(numbers, { it % 2 != 0 })`: This line uses the `findMatches` method to search for odd numbers in the `numbers` list.

11. `println oddNumbers`: This line prints the list of odd numbers to the console.

12. `def primeNumbers = Search.findMatches(numbers, { ... })`: This line uses a more complex lambda expression to search for prime numbers in the `numbers` list.

   - `if(it == 1) { return false }`: This line handles the case where the number being checked is 1, as 1 is not prime.
   - `boolean isPrime = true`: This line initializes a flag to keep track of whether the number is prime.
   - `for(int i = 2; i < it/2 + 1; i++) { ... }`: This loop iterates through all numbers from 2 to half of the number being checked, checking for divisibility.
   - `if(it % i == 0) { isPrime = false }`: If the number is divisible by any number in the range, it is not prime, so the flag is set to false.
   - `return isPrime`: Finally, the flag is returned to indicate whether the number is prime.

13. `println primeNumbers`: This line prints the list of prime numbers to the console.

This Groovy code demonstrates several advanced features, including static methods, lambda expressions, and complex conditional logic. It provides a flexible and concise way to search for and filter data based on specific criteria.