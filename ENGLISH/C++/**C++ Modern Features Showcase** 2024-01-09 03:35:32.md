```c++
#include <iostream>
#include <vector>

using namespace std;

// Define a custom exception class
class MyException : public exception {
public:
    virtual const char* what() const throw() {
        return "This is a custom exception!";
    }
};

// A function that throws a custom exception
void throw_custom_exception() {
    throw MyException();
}

// A function that uses a lambda expression
void use_lambda_expression() {
    auto sum = [](int a, int b) { return a + b; };
    cout << "Sum of 10 and 20 using lambda expression: " << sum(10, 20) << endl;
}

// A function that uses a variadic template
template <typename... Args>
void print_args(Args... args) {
    (cout << ... << args) << endl;
}

// A function that uses a range-based for loop
void use_range_based_for_loop(vector<int> numbers) {
    for (int number : numbers) {
        cout << number << " ";
    }
    cout << endl;
}

// A function that uses a structured binding declaration
void use_structured_binding_declaration(pair<string, int> person) {
    auto [name, age] = person;
    cout << "Name: " << name << ", Age: " << age << endl;
}

int main() {
    // Try-catch block to handle custom exception
    try {
        throw_custom_exception();
    } catch (const MyException& e) {
        cout << "Caught custom exception: " << e.what() << endl;
    }

    // Use lambda expression
    use_lambda_expression();

    // Use variadic template
    print_args(1, 2, 3, 4, 5);

    // Use range-based for loop
    vector<int> numbers = {1, 2, 3, 4, 5};
    use_range_based_for_loop(numbers);

    // Use structured binding declaration
    pair<string, int> person = {"John Doe", 30};
    use_structured_binding_declaration(person);

    return 0;
}
```

Explanation:

1. Custom Exception: A custom exception class `MyException` is defined and a function `throw_custom_exception()` is used to throw this exception. In the `main()` function, a try-catch block is used to catch the custom exception and print a message.

2. Lambda Expression: A lambda expression is used to define a function that calculates the sum of two numbers. It is stored in the variable `sum` and then invoked with the arguments 10 and 20. The result is printed to the console.

3. Variadic Template: A variadic template function `print_args()` is defined, which can take any number of arguments. It prints all the arguments to the console. In the `main()` function, it is called with five arguments.

4. Range-Based For Loop: A range-based for loop is used to iterate over a vector of integers. It prints each integer to the console.

5. Structured Binding Declaration: A structured binding declaration is used to unpack a pair of values into separate variables. In the `main()` function, a pair containing a name and an age is unpacked into two variables, and then printed to the console.