```d
import std.stdio, std.algorithm, std.range, std.string,
       std.traits, std.math, std.container, std.typecons;

void main() {
    auto friendlyGreetings =  ["Hello, world!",
                             "¡Hola, mundo!",
                             "你好，世界！",
                             "Γειά σου κόσμε!",
                             "مرحبا، العالم!",
                             "こんにちは世界！",
                             "你好，世界！",
                             "नमस्ते दुनिया!",
                             "Merhaba, dünya!",
                             "你好，世界！"];

    // Iterate over the array, print each element
    foreach (element; friendlyGreetings) {
        writefln("%s", element);
    }

    auto sortTypes = [](in T a, in T b) pure nothrow {
        return a < b;
    };
    static assert(isPure(sortTypes));
    static assert(isNothrow(sortTypes));

    // Define a custom comparator function for sorting
    auto invertedSortTypes = [](in T a, in T b) pure nothrow {
        return b < a;
    };
    static assert(isPure(invertedSortTypes));
    static assert(isNothrow(invertedSortTypes));

    // Initialize an array of integers
    auto numericValues = [1, 2, 5, 3, 4, 7, 8, 6, 9, 10];

    // Sort the array using the custom inverted comparator function
    sort(numericValues, invertedSortTypes);

    // Iterate over the sorted array and print each element
    foreach (element; numericValues) {
        writefln("%d", element);
    }

    // Generate a range of characters
    auto characterRange = 'a'..'z';

    // Convert the character range into a string
    auto generatedString = characterRange.map!('c').reduce!(+);

    // Print the generated string
    writefln("Generated String: %s", generatedString);

    // Define a struct for student data
    struct Student {
        string name;
        int age;
        int grade;
    };

    // Initialize an array of students
    auto students = [
        Student("Alice", 20, 88),
        Student("Bob", 22, 92),
        Student("Carol", 19, 95),
        Student("Dave", 21, 82),
        Student("Eve", 23, 98),
    ];

    // Sort the students by their grade in ascending order
    sort(students, [](in Student a, in Student b) pure nothrow {
        return a.grade < b.grade;
    });

    // Iterate over the sorted students and print each student's name and grade
    foreach (student; students) {
        writefln("%s: %d", student.name, student.grade);
    }

    // Check whether all students are passing (grade >= 60)
    auto allPassing = students.all!(s => s.grade >= 60);

    // Print the result of the check
    writefln("All students passing: %s", allPassing ? "Yes" : "No");

    // Calculate the average grade of the students
    auto averageGrade =
        students.map!('g').sum!() / students.length;

    // Print the average grade
    writefln("Average Grade: %.2f", averageGrade);

    // Find the best student (highest grade) using binary search
    static assert(isComparison(sortTypes));
    auto bestStudentIndex = binarySearch(students,
                                      Student(null, 0, 100), sortTypes);

    // Print the name and grade of the best student
    writefln("Best Student:");
    writefln("\tName: %s", students[bestStudentIndex].name);
    writefln("\tGrade: %d", students[bestStudentIndex].grade);
}
```

This D code demonstrates more advanced and varied features of the language. It starts by printing an array of friendly greetings in different languages, highlighting D's internationalization capabilities.

It also defines a custom comparator function and uses it to sort an array of integers in descending order, showcasing D's flexibility in handling sorting scenarios.

The code then generates a string from a range of characters and demonstrates working with strings.

It defines a struct for student data and operates on an array of students, including sorting them by grade, checking if all students are passing, and calculating the average grade.

Additionally, it uses binary search to find the best student (highest grade) in the array.

Finally, it prints out various information about the students and their grades, showcasing D's rich set of features for data manipulation and analysis.