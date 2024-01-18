```csharp
// This C# code demonstrates the use of nested classes, delegates, and lambda expressions to create a complex and differentiated program.

using System;
using System.Collections.Generic;

namespace ComplexCode
{
    // A nested class to represent a student
    public class Student
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public double GPA { get; set; }

        // A nested class to represent a student's address
        public class Address
        {
            public string StreetAddress { get; set; }
            public string City { get; set; }
            public string State { get; set; }
            public string ZipCode { get; set; }
        }
    }

    // A delegate to represent a method that takes a student as an argument and returns a boolean
    public delegate bool StudentFilter(Student student);

    class Program
    {
        // A method that uses a lambda expression to filter a list of students based on a specified criteria
        public static List<Student> FilterStudents(List<Student> students, StudentFilter filter)
        {
            List<Student> filteredStudents = new List<Student>();
            foreach (Student student in students)
            {
                if (filter(student))
                {
                    filteredStudents.Add(student);
                }
            }
            return filteredStudents;
        }

        static void Main(string[] args)
        {
            // Create a list of students
            List<Student> students = new List<Student>()
            {
                new Student() { Name = "John Doe", Age = 20, GPA = 3.5 },
                new Student() { Name = "Jane Smith", Age = 21, GPA = 4.0 },
                new Student() { Name = "Michael Jones", Age = 22, GPA = 3.8 },
                new Student() { Name = "Sarah Miller", Age = 19, GPA = 3.2 }
            };

            // Filter the students by age using a lambda expression
            List<Student> studentsOver20 = FilterStudents(students, (student) => student.Age > 20);

            // Print the names of the students over 20
            Console.WriteLine("Students over 20:");
            foreach (Student student in studentsOver20)
            {
                Console.WriteLine(student.Name);
            }

            // Filter the students by GPA using a lambda expression
            List<Student> studentsWithHighGPA = FilterStudents(students, (student) => student.GPA > 3.5);

            // Print the names of the students with a GPA over 3.5
            Console.WriteLine("\nStudents with GPA over 3.5:");
            foreach (Student student in studentsWithHighGPA)
            {
                Console.WriteLine(student.Name);
            }
        }
    }
}
```

Explanation:

1. Nested Classes:
   - The `Student` class is declared as a public class, which means it can be accessed from any other class in the program.
   - The `Address` class is declared as a nested class within the `Student` class, which means it can only be accessed from within the `Student` class.
   - This allows us to group related data together in a meaningful way.

2. Delegate:
   - The `StudentFilter` delegate is declared to represent a method that takes a `Student` object as an argument and returns a boolean value.
   - This allows us to pass a method as an argument to another method, which is a powerful technique for creating flexible and reusable code.

3. Lambda Expressions:
   - Lambda expressions are used to define anonymous methods, which are small blocks of code that can be passed as arguments to methods or assigned to variables.
   - In this code, lambda expressions are used to define the filtering criteria for the `FilterStudents` method.

4. Filtering Students:
   - The `FilterStudents` method takes a list of students and a `StudentFilter` delegate as arguments.
   - It iterates through the list of students and applies the filter criteria specified by the delegate.
   - The filtered students are then returned as a new list.

5. Main Method:
   - The `Main` method is the entry point of the program.
   - It creates a list of students and then uses the `FilterStudents` method to filter the students by age and GPA using lambda expressions.
   - Finally, it prints the names of the students that meet the filtering criteria.

This code demonstrates the use of nested classes, delegates, and lambda expressions to create a complex and differentiated program that can be used to filter a list of students based on different criteria.