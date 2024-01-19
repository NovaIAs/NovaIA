```groovy
import groovy.util.logging.Slf4j

@Slf4j
class GroovyComplexCode {

  def processData(data) {
    log.info("Processing data: ${data}")

    def result = data.collect { it * 2 }
      .groupBy { it % 3 }
      .sort { it.key }
      .findAll { it.size() > 1 }

    log.info("Result: ${result}")
  }

  def complexCalculation(a, b, c) {
    log.info("Performing complex calculation with a: ${a}, b: ${b}, c: ${c}")

    def result = (a + b) * c
      .div(b - a)
      .pow(2)
      .sqrt()

    log.info("Result: ${result}")
  }

  def generateRandomNumbers(count) {
    log.info("Generating ${count} random numbers")

    def randoms = (1..count).collect {
      Math.random() * 100
    }

    log.info("Random numbers: ${randoms}")
  }

  def manipulateStrings(text) {
    log.info("Manipulating string: ${text}")

    def result = text.toUpperCase()
      .replaceAll("[^a-zA-Z0-9\\s]", "")
      .split()
      .reverse()
      .join(" ")

    log.info("Result: ${result}")
  }

  def parseJson(jsonString) {
    log.info("Parsing JSON string: ${jsonString}")

    def json = new groovy.json.JsonSlurper().parseText(jsonString)

    log.info("Parsed JSON: ${json}")
  }

  def invokeHttpRequest(url) {
    log.info("Invoking HTTP request to URL: ${url}")

    def response = new URL(url).openConnection().text

    log.info("Response: ${response}")
  }

  static void main(String[] args) {
    def groovyComplexCode = new GroovyComplexCode()

    groovyComplexCode.processData([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    groovyComplexCode.complexCalculation(2, 3, 5)
    groovyComplexCode.generateRandomNumbers(10)
    groovyComplexCode.manipulateStrings("Hello World! This is a Groovy complex code example.")
    groovyComplexCode.parseJson('{"name": "John Doe", "age": 30, "occupation": "Software Engineer"}')
    groovyComplexCode.invokeHttpRequest("https://groovy-lang.org/")
  }
}
```

Explanation:

1. `processData` method:
   - Demonstrates data processing using Groovy's functional programming features.
   - It takes a list of numbers, multiplies each number by 2, groups the results by their remainder when divided by 3, sorts the groups, and filters out groups with a size less than or equal to 1.

2. `complexCalculation` method:
   - Illustrates a complex mathematical calculation using Groovy's operators and arithmetic methods.
   - It takes three numbers as input, performs a series of arithmetic operations, and returns the result.

3. `generateRandomNumbers` method:
   - Generates a list of random numbers using Groovy's iteration and mathematical functions.
   - It creates a range of numbers from 1 to the specified count, generates a random number for each number in the range, and collects the random numbers into a list.

4. `manipulateStrings` method:
   - Demonstrates string manipulation techniques in Groovy.
   - It converts the input string to uppercase, removes non-alphanumeric characters, splits the string into words, reverses the order of the words, and joins them back together with a space separator.

5. `parseJson` method:
   - Utilizes Groovy's JSON parsing capabilities to convert a JSON string into a Groovy object.
   - It uses the `JsonSlurper` class to parse the JSON string and returns the resulting object.

6. `invokeHttpRequest` method:
   - Demonstrates how to make an HTTP GET request using Groovy's built-in networking features.
   - It connects to the specified URL, retrieves the response, and returns the response text.

The main method at the bottom of the script creates an instance of the `GroovyComplexCode` class and calls the various methods to demonstrate their functionality.

This code showcases Groovy's versatility and ability to handle complex data processing, mathematical calculations, string manipulation, JSON parsing, and HTTP requests. It also demonstrates the use of Groovy's functional programming features, operators, and built-in methods to achieve complex tasks concisely and effectively.