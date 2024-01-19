```rust
// Create a struct to represent a student.
#[derive(Debug)]
struct Student {
    name: String,
    age: u8,
    grades: Vec<f32>,
}

// Create a function to calculate the average grade of a student.
fn calculate_average_grade(student: &Student) -> f32 {
    let sum_of_grades: f32 = student.grades.iter().sum();
    let number_of_grades: f32 = student.grades.len() as f32;
    sum_of_grades / number_of_grades
}

// Create a function to find the top student in a list of students.
fn find_top_student(students: &[Student]) -> &Student {
    let mut top_student = &students[0];
    for student in students {
        if calculate_average_grade(student) > calculate_average_grade(top_student) {
            top_student = student;
        }
    }
    top_student
}

// Create a function to print the name and average grade of a student.
fn print_student_info(student: &Student) {
    println!("{} has an average grade of {:.2}", student.name, calculate_average_grade(student));
}

// Create a list of students.
let students = vec![
    Student {
        name: "Alice".to_string(),
        age: 20,
        grades: vec![90.0, 85.0, 95.0],
    },
    Student {
        name: "Bob".to_string(),
        age: 21,
        grades: vec![80.0, 90.0, 85.0],
    },
    Student {
        name: "Charlie".to_string(),
        age: 22,
        grades: vec![75.0, 80.0, 90.0],
    },
];

// Find the top student in the list of students.
let top_student = find_top_student(&students);

// Print the name and average grade of the top student.
print_student_info(top_student);
```

Explanation:

1. **Struct Definition**:
   - We define a `Student` struct to represent a student with fields like `name`, `age`, and `grades`.

2. **Function to Calculate Average Grade**:
   - The `calculate_average_grade` function takes a `Student` reference and returns the average grade by summing up all grades and dividing by the number of grades.

3. **Function to Find Top Student**:
   - The `find_top_student` function takes a slice of `Student` structs as input and returns a reference to the student with the highest average grade.

4. **Function to Print Student Information**:
   - The `print_student_info` function takes a `Student` reference and prints the student's name and average grade.

5. **List of Students**:
   - We create a vector of `Student` structs to represent a list of students with names, ages, and grades.

6. **Finding the Top Student**:
   - We use the `find_top_student` function to find the student with the highest average grade from the list.

7. **Printing the Top Student's Information**:
   - We use the `print_student_info` function to print the name and average grade of the top student.

This code showcases various programming concepts, including structs, functions, slices, and loops. It demonstrates how to work with complex data structures and perform operations on them to find meaningful insights, in this case, the top student from a list of students based on their average grades.