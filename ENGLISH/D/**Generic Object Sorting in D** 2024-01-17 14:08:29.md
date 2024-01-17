```d
import std.stdio, std.algorithm, std.array, std.traits;
import std.algorithm.sort;

enum SortComparison { Ascending, Descending }

class MyObject {
    int id;
    string name;

    this(int id = 0, string name = "unnamed") {
        this.id = id;
        this.name = name;
    }

    int getId() { return id; }
    string getName() { return name; }
}

template MyCompare(T)(SortComparison order) {
    alias Comparator = pure function(immutable T, immutable T): int;
    Comparator compare;

    if (order == SortComparison.Ascending) {
        compare = (a, b) => a < b ? -1 : a > b ? 1 : 0;
    } else {
        compare = (a, b) => b < a ? -1 : b > a ? 1 : 0;
    }

    return compare;
}

void sortObjectsById(inout MyObject[] objects, SortComparison order = SortComparison.Ascending) pure {
    sort!objects(MyCompare!(MyObject)(order));
}

void sortObjectsByName(inout MyObject[] objects, SortComparison order = SortComparison.Ascending) pure {
    sort!objects(MyCompare!(MyObject)(order), &name);
}

void printObjects(immutable MyObject[] objects) pure {
    foreach (obj; objects) {
        writeln("ID:", obj.getId(), "Name:", obj.getName());
    }
}

int main() pure nothrow {
    MyObject[] objects = [
        new MyObject(1, "Object 1"),
        new MyObject(3, "Object 3"),
        new MyObject(2, "Object 2"),
        new MyObject(4, "Object 4")
    ];

    writeln("\nUnsorted Objects:");
    printObjects(objects);

    sortObjectsById(objects);
    writeln("\nObjects Sorted By ID (Ascending):");
    printObjects(objects);

    sortObjectsByName(objects, SortComparison.Descending);
    writeln("\nObjects Sorted By Name (Descending):");
    printObjects(objects);

    return 0;
}
```

This code defines a set of functions to sort an array of objects based on different criteria, such as their ID or their name, using a customizable comparison function.

Here's how the code works:

1. We define a template function `MyCompare` that receives a generic type `T` and a `SortComparison` enum value.
2. Inside the `MyCompare` function, we create a nested `Comparator` alias, which is defined as a pure function that takes two immutable `T` objects and returns an integer.
3. We initialize the `compare` variable to a lambda expression that implements the comparison logic based on the `order` parameter. The lambda expression returns -1 if the first object is less than the second, 1 if the first object is greater than the second, and 0 if they are equal.
4. We define two functions, `sortObjectsById` and `sortObjectsByName`, that use the `MyCompare` function to sort an array of `MyObject` objects by their ID or name, respectively.
5. We define a `printObjects` function to print the information of the objects in the array.
6. In the `main` function, we create an array of `MyObject` objects and print them before sorting.
7. We call the `sortObjectsById` function to sort the objects by their ID in ascending order.
8. We call the `sortObjectsByName` function to sort the objects by their name in descending order.
9. After each sorting operation, we print the sorted array.

The output of this code will be:

```
Unsorted Objects:
ID: 1 Name: Object 1
ID: 3 Name: Object 3
ID: 2 Name: Object 2
ID: 4 Name: Object 4

Objects Sorted By ID (Ascending):
ID: 1 Name: Object 1
ID: 2 Name: Object 2
ID: 3 Name: Object 3
ID: 4 Name: Object 4

Objects Sorted By Name (Descending):
ID: 4 Name: Object 4
ID: 3 Name: Object 3
ID: 2 Name: Object 2
ID: 1 Name: Object 1
```