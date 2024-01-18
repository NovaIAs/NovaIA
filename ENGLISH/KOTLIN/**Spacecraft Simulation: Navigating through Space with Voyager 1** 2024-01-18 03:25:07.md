```
class Spacecraft(val name: String, var speed: Double, var course: Double, var fuelLevel: Double) {

    // Define the properties of the spacecraft
    val maxSpeed = 1000.0
    val maxFuelLevel = 100.0

    // Function to accelerate the spacecraft
    fun accelerate() {
        if (speed < maxSpeed && fuelLevel > 0) {
            speed += 50.0
            fuelLevel -= 1.0
        }
    }

    // Function to decelerate the spacecraft
    fun decelerate() {
        if (speed > 0) {
            speed -= 50.0
        }
    }

    // Function to change the course of the spacecraft
    fun changeCourse(newCourse: Double) {
        course = newCourse
    }

    // Function to refuel the spacecraft
    fun refuel(amount: Double) {
        if (fuelLevel + amount <= maxFuelLevel) {
            fuelLevel += amount
        } else {
            fuelLevel = maxFuelLevel
        }
    }

    // Function to display the status of the spacecraft
    fun displayStatus() {
        println("Name: $name")
        println("Speed: $speed")
        println("Course: $course")
        println("Fuel Level: $fuelLevel")
    }
}

fun main(args: Array<String>) {

    // Create an instance of the Spacecraft class
    val voyager1 = Spacecraft("Voyager 1", 0.0, 0.0, 100.0)

    // Accelerate the spacecraft
    voyager1.accelerate()

    // Change the course of the spacecraft
    voyager1.changeCourse(45.0)

    // Decelerate the spacecraft
    voyager1.decelerate()

    // Refuel the spacecraft
    voyager1.refuel(20.0)

    // Display the status of the spacecraft
    voyager1.displayStatus()
}
```

Explanation:

1. We define a `Spacecraft` class with properties like name, speed, course, and fuel level. It also has functions to accelerate, decelerate, change course, refuel, and display status.

2. In the `main()` function, we create an instance of the `Spacecraft` class named `voyager1` and set its initial values.

3. We then call various functions on the `voyager1` object to accelerate, change course, decelerate, refuel, and display its status.

4. This code showcases object-oriented programming concepts like encapsulation, inheritance, and polymorphism. It demonstrates a more complex program with multiple functions and objects interacting with each other.