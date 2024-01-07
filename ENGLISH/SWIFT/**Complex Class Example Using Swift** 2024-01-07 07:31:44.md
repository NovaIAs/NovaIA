```swift
import Foundation

class ComplexClass {
    
    // MARK: - Properties
    
    private let complexData: [String: Any]
    private let complexLogic: (String) -> String
    
    // MARK: - Initialization
    
    init(complexData: [String: Any], complexLogic: @escaping (String) -> String) {
        self.complexData = complexData
        self.complexLogic = complexLogic
    }
    
    // MARK: - Methods
    
    func doSomethingComplex() -> String {
        var result = ""
        
        for (key, value) in complexData {
            result += "\(key): \(value)\n"
        }
        
        result += complexLogic("Hello, world!")
        
        return result
    }
}

// MARK: - Example Usage

let complexData = ["name": "John Doe", "age": 30, "city": "New York"]

let complexLogic: (String) -> String = { input in
    return input.uppercased()
}

let complexClass = ComplexClass(complexData: complexData, complexLogic: complexLogic)

let result = complexClass.doSomethingComplex()

print(result)
```

**Explanation:**

This code defines a complex class called `ComplexClass` that has a complex data structure and a complex logic function. The class has a private property called `complexData` which is a dictionary that contains complex data, and a private property called `complexLogic` which is a function that takes a string as input and returns a string as output.

The class has an initializer that takes two parameters: `complexData` and `complexLogic`, and sets the corresponding private properties.

The class has a method called `doSomethingComplex()` that uses the complex data and the complex logic to generate a string result. The method loops through the `complexData` dictionary and appends the key and value of each entry to the result string. Then, it calls the `complexLogic` function with the string "Hello, world!" as input and appends the result to the result string. Finally, it returns the result string.

The code then creates an instance of the `ComplexClass` class using the example data and logic provided. It calls the `doSomethingComplex()` method on the instance and prints the result to the console.

The output of the code will be:

```
name: John Doe
age: 30
city: New York
HELLO, WORLD!
```