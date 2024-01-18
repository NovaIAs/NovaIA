// This Swift code defines a complex and differentiated function that calculates various mathematical operations on a given input number. It combines multiple mathematical concepts and demonstrates the versatility of the language.

// Define the function with its input parameter and return type
func complexFunction(input: Double) -> (sum: Double, product: Double, quotient: Double, remainder: Double, power: Double, factorial: Double, squareRoot: Double) {

    // Calculate the sum of the input and 10.0
    let sum = input + 10.0

    // Calculate the product of the input and 5.0
    let product = input * 5.0

    // Calculate the quotient by dividing the input by 2.0
    let quotient = input / 2.0

    // Calculate the remainder by taking the modulo of the input and 3.0
    let remainder = input.remainder(dividingBy: 3.0)

    // Calculate the power by raising the input to the power of 3
    let power = pow(input, 3)

    // Calculate the factorial of the input using a recursive helper function
    func factorialHelper(number: Double) -> Double {
        if number == 0 {
            return 1
        } else {
            return number * factorialHelper(number: number - 1)
        }
    }

    let factorial = factorialHelper(number: input)

    // Calculate the square root of the input
    let squareRoot = sqrt(input)

    // Return a tuple containing all the calculated values
    return (sum, product, quotient, remainder, power, factorial, squareRoot)
}

// Example usage: Call the function with different input values and print the results
let input1 = 5.0
let result1 = complexFunction(input: input1)
print("Input: \(input1)")

print("Sum: \(result1.sum)")
print("Product: \(result1.product)")
print("Quotient: \(result1.quotient)")
print("Remainder: \(result1.remainder)")
print("Power: \(result1.power)")
print("Factorial: \(result1.factorial)")
print("Square Root: \(result1.squareRoot)")

let input2 = 12.5
let result2 = complexFunction(input: input2)
print("\nInput: \(input2)")

print("Sum: \(result2.sum)")
print("Product: \(result2.product)")
print("Quotient: \(result2.quotient)")
print("Remainder: \(result2.remainder)")
print("Power: \(result2.power)")
print("Factorial: \(result2.factorial)")
print("Square Root: \(result2.squareRoot)")