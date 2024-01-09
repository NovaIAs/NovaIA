```groovy
// This groovy script is a complex and highly differentiated code that showcases various features and capabilities of the language.

// Define a method to calculate the factorial of a number
def factorial(n) {
  if (n == 0) {
    return 1
  } else {
    return n * factorial(n-1)
  }
}

// Define a class representing a complex number
class Complex {
  double real
  double imaginary

  // Constructor to initialize the complex number
  Complex(double real, double imaginary) {
    this.real = real
    this.imaginary = imaginary
  }

  // Define a method to add two complex numbers
  Complex add(Complex other) {
    new Complex(this.real + other.real, this.imaginary + other.imaginary)
  }

  // Define a method to multiply two complex numbers
  Complex multiply(Complex other) {
    new Complex(this.real * other.real - this.imaginary * other.imaginary,
                 this.real * other.imaginary + this.imaginary * other.real)
  }

  // Define a method to convert the complex number to a string
  String toString() {
    "${real} + ${imaginary}i"
  }
}

// Define a closure to check if a number is prime
def isPrime = { number ->
  if (number <= 1) {
    return false
  }
  for (i in 2..<number) {
    if (number % i == 0) {
      return false
    }
  }
  return true
}

// Define a list of prime numbers
def primeNumbers = []
for (i in 2..100) {
  if (isPrime(i)) {
    primeNumbers << i
  }
}

// Use a GString to concatenate strings and expressions
def greeting = "Hello, ${System.getProperty('user.name')}!"

// Use the spread operator to pass an array as arguments to a method
def sumOfPrimeNumbers = primeNumbers.sum()

// Use the Elvis operator to assign a default value if a variable is null
def defaultValue = "World"
def name = System.getProperty('name') ?: defaultValue

// Use the ternary operator to conditionally assign a value to a variable
def message = name == 'World' ? 'Hello, World!' : "Hello, ${name}!"

// Use the ?: operator to assign a default value if a condition is false
def result = 10 / 0 ?: 'Division by zero is undefined'

// Use the assert statement to check for conditions and throw an exception if they are not met
assert 1 + 1 == 2, '1 + 1 should equal 2'

// Use the try-catch block to handle exceptions
try {
  10 / 0
} catch (ArithmeticException e) {
  println 'An arithmetic exception occurred: ' + e.message
}

// Use the switch statement to conditionally execute blocks of code
switch (name) {
  case 'Alice':
    println 'Hello, Alice!'
    break
  case 'Bob':
    println 'Hello, Bob!'
    break
  default:
    println "Hello, ${name}!"
}

// Use the for-each loop to iterate over a collection
primeNumbers.each { prime ->
  println prime
}

// Use the while loop to execute a block of code while a condition is true
while (sumOfPrimeNumbers < 1000) {
  sumOfPrimeNumbers += primeNumbers.pop()
}

// Use the do-while loop to execute a block of code at least once
do {
  sumOfPrimeNumbers -= primeNumbers.pop()
} while (sumOfPrimeNumbers > 0)

// Use the break statement to exit a loop early
for (i in 1..10) {
  if (i == 5) {
    break
  }
  println i
}

// Use the continue statement to skip the rest of the current iteration of a loop
for (i in 1..10) {
  if (i % 2 == 0) {
    continue
  }
  println i
}

// Use the return statement to exit a method early
def square(n) {
  if (n < 0) {
    return 'Negative numbers cannot be squared'
  }
  return n * n
}

// Use the throw statement to throw an exception
def divide(a, b) {
  if (b == 0) {
    throw new ArithmeticException('Division by zero is undefined')
  }
  return a / b
}

// Use the synchronized block to ensure that only one thread can access a shared resource at a time
def sharedResource = 0
synchronized (sharedResource) {
  sharedResource++
}

// Use the wait() and notify() methods to implement inter-thread communication
def lock = new Object()
def condition = lock.newCondition()
synchronized (lock) {
  condition.await()
  condition.signal()
}

// Use the Java interoperability features to call Java methods and classes
def javaList = new ArrayList()
javaList.add('Hello')
javaList.add('World')
javaList.each { item ->
  println item
}

// Use the GroovyBeansBuilder to dynamically create Java objects
def builder = new GroovyBeansBuilder()
def person = builder.build {
  name 'John Doe'
  age 30
}

println person.name
println person.age

// Use the GroovyShell to dynamically execute Groovy code
def shell = new GroovyShell()
def script = '''
def message = 'Hello, Groovy!'
println message
'''
shell.evaluate script

// Use the GroovyClassLoader to dynamically load and execute Groovy classes
def classLoader = new GroovyClassLoader()
def groovyClass = classLoader.parseClass '''
class MyClass {
  def sayHello() {
    println 'Hello from Groovy class!'
  }
}
'''
def instance = groovyClass.newInstance()
instance.sayHello()
```