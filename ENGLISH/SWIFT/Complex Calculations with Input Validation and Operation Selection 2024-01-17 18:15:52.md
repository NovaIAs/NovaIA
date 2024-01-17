```swift
class ComplexCalculation {

    // Properties
    private var inputValues: [Double]
    private var operation: CalculationOperation

    // Initialization
    init(inputValues: [Double], operation: CalculationOperation) {
        self.inputValues = inputValues
        self.operation = operation
    }

    // Function to perform the complex calculation
    func performCalculation() -> Double {
        var result: Double = 0.0

        // Execute the calculation based on the operation
        switch operation {
        case .Addition:
            for value in inputValues {
                result += value
            }
            break
        case .Subtraction:
            if inputValues.count < 2 {
                print("Error: Insufficient input values for subtraction.")
                return 0.0
            }
            result = inputValues[0]
            for i in 1..<inputValues.count {
                result -= inputValues[i]
            }
            break
        case .Multiplication:
            for value in inputValues {
                result *= value
            }
            break
        case .Division:
            if inputValues.count < 2 {
                print("Error: Insufficient input values for division.")
                return 0.0
            }
            result = inputValues[0]
            for i in 1..<inputValues.count {
                if inputValues[i] != 0 { result /= inputValues[i] }
                else { print("Error: Division by zero encountered."); return 0.0 }
            }
            break
        case .Exponentiation:
            if inputValues.count < 2 {
                print("Error: Insufficient input values for exponentiation.")
                return 0.0
            }
            result = pow(inputValues[0], inputValues[1])
            break
        case .Logarithm:
            if inputValues.count != 2 {
                print("Error: Invalid number of input values for logarithm.")
                return 0.0
            }
            if inputValues[0] <= 0 || inputValues[1] <= 0 || inputValues[1] == 1 {
                print("Error: Invalid input values for logarithm.")
                return 0.0
            }
            result = log(inputValues[0]) / log(inputValues[1])
            break
        }

        return result
    }
}

// Define the calculation operation enum
enum CalculationOperation {
    case Addition, Subtraction, Multiplication, Division, Exponentiation, Logarithm
}

// Example usage:
let inputValues1 = [1.0, 2.0, 3.0]
let inputValues2 = [4.0, 2.0]
let operation1 = CalculationOperation.Addition
let operation2 = CalculationOperation.Division

let complexCalculation1 = ComplexCalculation(inputValues: inputValues1, operation: operation1)
let result1 = complexCalculation1.performCalculation()
print("Result of calculation 1:", result1)

let complexCalculation2 = ComplexCalculation(inputValues: inputValues2, operation: operation2)
let result2 = complexCalculation2.performCalculation()
print("Result of calculation 2:", result2)
```

Explanation:

1. The `ComplexCalculation` class is defined with properties for input values and the calculation operation to be performed.
2. The `performCalculation()` method is responsible for executing the calculation based on the operation.
3. The `CalculationOperation` enum defines the supported operations (addition, subtraction, multiplication, division, exponentiation, and logarithm).
4. In the example usage, two sets of input values and operations are provided.
5. Two `ComplexCalculation` instances are created with these inputs and operations.
6. The `performCalculation()` method is called on each instance to calculate the result.
7. The results are printed to the console.

This code is complex because it performs various calculations based on the provided operation and input values. It also includes error handling for invalid input values or operations. The code demonstrates the use of classes, methods, and enums in a complex calculation scenario.