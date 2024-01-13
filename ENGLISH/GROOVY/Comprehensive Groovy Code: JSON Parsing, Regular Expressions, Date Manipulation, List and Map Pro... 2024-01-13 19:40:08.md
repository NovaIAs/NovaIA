```groovy
import groovy.json.JsonSlurperClassic
import groovy.time.TimeCategory

@groovy.transform.CompileStatic
class ComplexGroovyCode {

    static void main(String[] args) {
        // Json Parsing
        def jsonSlurper = new JsonSlurperClassic()
        def jsonObject = jsonSlurper.parseText('{ "name": "John Doe", "age": 30, "address": { "street": "123 Main St", "city": "Anytown", "state": "CA", "zip": "12345" } }')
        println jsonObject.name
        println jsonObject.age
        println jsonObject.address.street

        // Regular Expressions
        def pattern = ~/(\d{4})-(\d{2})-(\d{2})/
        def matcher = pattern.matcher("2023-03-08")
        if (matcher.matches()) {
            println "Date: ${matcher.group(1)}-${matcher.group(2)}-${matcher.group(3)}"
        }

        // Date and Time Manipulation
        def now = new Date()
        use(TimeCategory) {
            println now + 1.day
            println now.minus(2.hours)
        }

        // List Processing
        def numbers = [1, 2, 3, 4, 5]
        println numbers.sum()
        println numbers.max()
        println numbers.findAll { it % 2 == 0 }

        // Map Processing
        def capitals = [
                "USA": "Washington D.C.",
                "UK": "London",
                "France": "Paris"
        ]
        println capitals.get("USA")
        println capitals.keySet()
        println capitals.values()

        // Closures
        def add = { a, b -> a + b }
        println add(1, 2)

        // Recursion
        def factorial = { n -> n <= 1 ? 1 : n * factorial(n - 1) }
        println factorial(5)

        // Metaprogramming
        def metaClass = new ExpandoMetaClass(Integer)
        metaClass.isEven = { -> delegate % 2 == 0 }
        println 10.isEven()

        // Concurrency
        def executorService = Executors.newFixedThreadPool(4)
        def futures = []
        for (i in 1..10) {
            futures << executorService.submit({ -> println "Task $i" })
        }
        futures.each { it.get() }
        executorService.shutdown()

        // Groovy Truth
        println true && false
        println true || false
        println !true
    }
}
```

Explanation:

1. JSON Parsing: Parse and manipulate JSON data using the JsonSlurper class.

2. Regular Expressions: Use regular expressions to match and extract data from strings.

3. Date and Time Manipulation: Use the TimeCategory Groovy extension to easily manipulate dates and times.

4. List Processing: Perform common operations on lists such as finding the sum, maximum value, and filtering elements.

5. Map Processing: Work with maps to store and retrieve data efficiently.

6. Closures: Use closures to represent anonymous functions and pass them as arguments to other methods.

7. Recursion: Implement recursive algorithms using Groovy's concise syntax.

8. Metaprogramming: Dynamically modify the behavior of classes and objects using metaprogramming techniques.

9. Concurrency: Utilize the Java ExecutorService to execute tasks concurrently and improve performance.

10. Groovy Truth: Understand the unique truth values in Groovy, where values like empty strings and null are considered false, while non-empty strings and non-null values are considered true.