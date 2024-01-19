```d
import std.stdio, std.algorithm, std.container;

class MyClass {
    immutable int value;
    immutable int id;
    immutable string name;

    // Constructor
    this(int value, int id, string name) {
        this.value = value;
        this.id = id;
        this.name = name;
    }

    // Getters
    int getValue() const { return value; }
    int getId() const { return id; }
    string getName() const { return name; }

    // Comparison operators
    bool operator==(const MyClass& other) const { return this.value == other.value && this.id == other.id && this.name == other.name; }
    bool operator!=(const MyClass& other) const { return !operator==(other); }

    // Output operator
    void write(out) const {
        out.writefln("MyClass(value: %d, id: %d, name: %s)", value, id, name);
    }
};

void main() {
    // Create a vector of MyClass objects
    auto vec = [new MyClass(1, 2, "a"), new MyClass(3, 4, "b"), new MyClass(5, 6, "c")];

    // Print the vector
    writeln("Vector:");
    for (auto obj : vec) {
        obj.write();
    }

    // Sort the vector by the value field
    vec.sort!(a, b) { a.getValue() < b.getValue(); };

    // Print the sorted vector
    writeln("Sorted vector:");
    for (auto obj : vec) {
        obj.write();
    }

    // Find the MyClass object with the name "b"
    auto found = vec.findLast!(obj) { obj.getName() == "b"; };

    // Print the found object
    writeln("Found object:");
    found.write();

    // Remove the found object from the vector
    vec.remove!(found);

    // Print the remaining vector
    writeln("Remaining vector:");
    for (auto obj : vec) {
        obj.write();
    }

    // Free the memory allocated for the MyClass objects
    vec.free();
}
```

Explanation:

1. Define a class `MyClass` with three immutable fields: `value`, `id`, and `name`.

2. Define a constructor for the `MyClass` class that takes three arguments: `value`, `id`, and `name`.

3. Define getter methods for each field: `getValue()`, `getId()`, and `getName()`.

4. Define comparison operators `==` and `!=` for the class.

5. Define an output operator `write()` for the class that prints the object's fields.

6. In the `main()` function:
   - Create a vector `vec` of `MyClass` objects.
   - Print the vector.
   - Sort the vector by the `value` field.
   - Print the sorted vector.
   - Find the `MyClass` object with the name "b".
   - Print the found object.
   - Remove the found object from the vector.
   - Print the remaining vector.
   - Free the memory allocated for the `MyClass` objects.

This code demonstrates the following features of the D language:

- Classes and objects
- Immutable fields
- Constructor
- Getter methods
- Comparison operators
- Output operator
- Vector container
- Sorting
- Finding an element in a vector
- Removing an element from a vector
- Memory management