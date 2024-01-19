```rust
// Declare a structure to represent a student
#[derive(Debug)]
struct Student {
    name: String,
    age: u8,
    grades: Vec<f32>,
}

// Create a function to calculate the average grade of a student
fn calculate_average_grade(student: &Student) -> f32 {
    let sum_of_grades: f32 = student.grades.iter().sum();
    let number_of_grades: usize = student.grades.len();
    sum_of_grades / number_of_grades as f32
}

// Create a function to create a list of students from a CSV file
fn read_students_from_csv(filename: &str) -> Result<Vec<Student>, std::io::Error> {
    let mut reader = csv::Reader::from_path(filename)?;
    let mut students: Vec<Student> = Vec::new();

    for result in reader.deserialize() {
        let record: Student = result?;
        students.push(record);
    }

    Ok(students)
}

// Create a function to print the student data to the console
fn print_students(students: &[Student]) {
    for student in students {
        println!("{:?}", student);
    }
}

// Main function
fn main() {
    // Read the student data from a CSV file
    let students = read_students_from_csv("students.csv").unwrap();

    // Print the student data to the console
    print_students(&students);

    // Calculate the average grade for each student
    for student in &students {
        let average_grade = calculate_average_grade(student);
        println!(
            "{} has an average grade of {:.2}",
            student.name, average_grade
        );
    }
}
```

This code performs the following tasks:

1. It defines a structure named `Student` to represent a student's data, including their name, age, and grades.

2. It defines a function called `calculate_average_grade` that calculates the average grade of a student given their grades.

3. It defines a function called `read_students_from_csv` that reads student data from a CSV file and returns a vector of `Student` structs.

4. It defines a function called `print_students` that prints the student data to the console.

5. In the `main` function, it calls the `read_students_from_csv` function to read the student data from a CSV file.

6. It calls the `print_students` function to print the student data to the console.

7. It iterates over the list of students, calculates the average grade for each student using the `calculate_average_grade` function, and prints the student's name and average grade.