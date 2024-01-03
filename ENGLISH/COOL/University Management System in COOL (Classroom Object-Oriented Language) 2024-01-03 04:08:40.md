Certainly! Below is a complex and unique code written in COOL (Classroom Object-Oriented Language). This code represents a university management system that handles student registration, course enrollment, and grading. It consists of multiple classes and their respective methods. Please note that due to character limitations, the code may not be as extensive as you requested, but it still showcases the complexity and functionality of the system.

```cool
class Student {
  name: String
  id: Int
  courses: List<Course>
  
  def init(name: String, id: Int): SELF_TYPE {
    self.name <- name
    self.id <- id
    self.courses <- new List<Course>()
    self
  }
  
  def enroll(course: Course): Bool {
    if course.capacity > 0 then
      course.capacity <- course.capacity - 1
      courses.append(course)
      true
    else
      false
  }
  
  def drop(course: Course): Bool {
    if courses.contains(course) then
      course.capacity <- course.capacity + 1
      courses.remove(course)
      true
    else
      false
  }
  
  def viewGrades(): List<Grade> {
    let grades: List<Grade> <- new List<Grade>()
    for course in courses loop
      grades.append(course.getGrade(self))
    pool
    grades
  }
}

class Course {
  name: String
  code: String
  capacity: Int
  students: List<Student>
  grades: List<Grade>
  
  def init(name: String, code: String, capacity: Int): SELF_TYPE {
    self.name <- name
    self.code <- code
    self.capacity <- capacity
    self.students <- new List<Student>()
    self.grades <- new List<Grade>()
    self
  }
  
  def getGrade(student: Student): Grade {
    let grade: Grade <- grades.find(g | g.student = student)
    grade
  }
  
  def setGrade(student: Student, grade: Int): Bool {
    let existingGrade: Grade <- grades.find(g | g.student = student)
    if existingGrade = void then
      let newGrade <- new Grade(student, grade)
      grades.append(newGrade)
    else
      existingGrade.grade <- grade
    true
  }
  
  def register(student: Student): Bool {
    if students.length < capacity then
      students.append(student)
      true
    else
      false
  }
  
  def unregister(student: Student): Bool {
    if students.contains(student) then
      students.remove(student)
      true
    else
      false
  }
}

class Grade {
  student: Student
  grade: Int
  
  def init(student: Student, grade: Int): SELF_TYPE {
    self.student <- student
    self.grade <- grade
    self
  }
}

class University {
  students: List<Student>
  courses: List<Course>
  
  def init(): SELF_TYPE {
    self.students <- new List<Student>()
    self.courses <- new List<Course>()
    self
  }
  
  def registerStudent(student: Student): Bool {
    if !students.contains(student) then
      students.append(student)
      true
    else
      false
  }
  
  def unregisterStudent(student: Student): Bool {
    if students.contains(student) then
      students.remove(student)
      true
    else
      false
  }
  
  def addCourse(course: Course): Bool {
    if !courses.contains(course) then
      courses.append(course)
      true
    else
      false
  }
  
  def removeCourse(course: Course): Bool {
    if courses.contains(course) then
      courses.remove(course)
      true
    else
      false
  }
}
```

Explanation:
The code above represents a university management system implemented in COOL. It consists of four classes: `Student`, `Course`, `Grade`, and `University`.

- The `Student` class represents a university student. It has attributes such as `name`, `id`, and a `List` of enrolled `Course` objects.
- The `Course` class represents a course offered by the university. It has attributes such as `name`, `code`, `capacity` (maximum number of students), a `List` of enrolled `Student` objects, and a `List` of `Grade` objects.
- The `Grade` class represents a grade received by a student in a specific course. It has attributes `student` and `grade`.
- The `University` class serves as the main interface for managing students and courses. It has attributes `students` and `courses`, which are `List` objects containing all registered students and courses, respectively.

The code provides various methods to interact with the university management system. Some key methods include:

- `enroll(course: Course): Bool`: Allows a student to enroll in a course if there are available slots.
- `drop(course: Course): Bool`: Allows a student to drop a course they are currently enrolled in.
- `setGrade(student: Student, grade: Int): Bool`: Sets the grade for a student in a particular course.
- `register(student: Student): Bool`: Registers a student for a course if there are available slots.
- `unregister(student: Student): Bool`: Unregisters a student from a course they are currently registered in.
- `registerStudent(student: Student): Bool`: Registers a student with the university.
- `unregisterStudent(student: Student): Bool`: Unregisters a student from the university.
- `addCourse(course: Course): Bool`: Adds a course to the university's course list.
- `removeCourse(course: Course): Bool`: Removes a course from the university's course list.

This code forms a foundation for a comprehensive university management system, allowing students to enroll, drop courses, view their grades, and for administrators to manage student registrations and course offerings.