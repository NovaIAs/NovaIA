```prolog
:- dynamic professor/2, course/2, student/2, takes/3.

% Professors
professor(mary, "Computer Science").
professor(bob, "Mathematics").
professor(alice, "Physics").

% Courses
course(cs101, "Introduction to Computer Science").
course(cs201, "Data Structures").
course(cs301, "Algorithms").
course(math101, "Calculus I").
course(math201, "Calculus II").
course(math301, "Linear Algebra").
course(phys101, "Physics I").
course(phys201, "Physics II").
course(phys301, "Quantum Mechanics").

% Students
student(tom, "Computer Science").
student(mary, "Mathematics").
student(alice, "Physics").

% Takes
takes(tom, cs101, mary).
takes(tom, cs201, bob).
takes(tom, cs301, alice).
takes(mary, math101, bob).
takes(mary, math201, alice).
takes(mary, math301, mary).
takes(alice, phys101, bob).
takes(alice, phys201, alice).
takes(alice, phys301, mary).

% Rules
% A student is taking a course if they are enrolled in the course and the course is offered by a professor in their department.
taking(Student, Course) :-
    student(Student, Department),
    course(Course, _),
    professor(Professor, Department),
    takes(Student, Course, Professor).

% A student is enrolled in a course if they are taking the course and the course is offered in the current semester.
enrolled(Student, Course) :-
    taking(Student, Course),
    current_semester(Semester),
    course(Course, Semester).

% A student is a graduate if they have taken all of the courses in their department.
graduate(Student) :-
    student(Student, Department),
    course(Course, Department),
    enrolled(Student, Course).

% A professor is teaching a course if they are offering the course and at least one student is taking it.
teaching(Professor, Course) :-
    professor(Professor, Department),
    course(Course, Department),
    takes(_, Course, Professor).

% A course is offered in the current semester if it is listed in the course catalog for the semester.
current_semester(Semester) :-
    course_catalog(Semester, _).

% Facts
course_catalog(fall, [cs101, cs201, cs301, math101, math201, math301, phys101, phys201, phys301]).
course_catalog(spring, [cs101, cs202, cs302, math102, math202, math302, phys102, phys202, phys302]).
```

This code is a Prolog program that models a university course registration system. It includes information about professors, courses, students, and which courses students are taking. The program can be used to answer questions about who is taking what courses, who is teaching what courses, and whether a student is enrolled in a particular course.

The program uses a number of Prolog rules to define relationships between the different types of entities in the system. For example, the rule `taking(Student, Course)` defines the relationship between a student and a course that they are taking. The rule `enrolled(Student, Course)` defines the relationship between a student and a course that they are enrolled in. The rule `graduate(Student)` defines the relationship between a student and a degree that they have earned.

The program also includes a number of facts, which are statements that are true in the system. For example, the fact `professor(mary, "Computer Science")` states that Mary is a professor in the Computer Science department. The fact `course(cs101, "Introduction to Computer Science")` states that cs101 is a course in the Computer Science department.

The program can be used to answer questions about the system by using the Prolog query mechanism. For example, the query `taking(tom, cs101)` would return true if Tom is taking cs101. The query `enrolled(mary, math201)` would return true if Mary is enrolled in math201. The query `graduate(alice)` would return true if Alice has earned a degree.