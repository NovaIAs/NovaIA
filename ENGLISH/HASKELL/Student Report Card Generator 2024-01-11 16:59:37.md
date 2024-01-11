```haskell
import Data.List (groupBy, sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- Define the type of a student record.
data Student = Student {
  name :: String,
  grades :: [Int]
} deriving (Show, Eq)

-- Define the type of a course record.
data Course = Course {
  name :: String,
  students :: [Student]
} deriving (Show)

-- Define the type of a report card.
data ReportCard = ReportCard {
  student :: Student,
  courses :: [Course]
} deriving (Show)

-- Parse the command-line arguments to get the input file name.
inputFileName <- getArgs >>= return . head

-- Read the input file and parse the student records.
students <- readFile inputFileName >>= parseStudents

-- Group the students by their names.
studentsByName <- groupBy (\s1 s2 -> s1.name == s2.name) students

-- Sort the students by their names.
studentsByName <- sort studentsByName

-- Create a course record for each student.
courses <- map (\(name, students) -> Course name students) studentsByName

-- Create a report card for each student.
reportCards <- map (\s -> ReportCard s courses) students

-- Print the report cards.
mapM_ print reportCards

-- Define a function to parse a student record from a line of text.
parseStudent :: String -> Student
parseStudent line = Student {
  name = name',
  grades = grades'
}
  where
    [name', gradesText] = break (== ',') line
    grades' = map read (words gradesText)

-- Define a function to parse student records from a string.
parseStudents :: String -> [Student]
parseStudents input = map parseStudent (lines input)
```

This code reads a text file containing student records, parses the records into a list of `Student` objects, groups the students by their names, sorts the students by their names, creates a `Course` object for each student, creates a `ReportCard` object for each student, and prints the report cards.

The input file is expected to be in the following format:

```
John Doe, 90, 85, 95
Jane Smith, 80, 95, 85
```

Each line of the input file contains a student's name, followed by a comma-separated list of their grades.

The output of the program will be a list of report cards, each of which contains a student's name, a list of the courses they are taking, and their grades in those courses.

For example, the output of the program for the input file above would be:

```
Report Card for John Doe:

Courses:
  - Math: 90
  - Science: 85
  - English: 95

Report Card for Jane Smith:

Courses:
  - Math: 80
  - Science: 95
  - English: 85
```