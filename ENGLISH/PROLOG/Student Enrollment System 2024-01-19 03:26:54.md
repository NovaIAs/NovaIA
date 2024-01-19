```prolog
% Define a knowledge base of facts and rules.

% Facts:
student(john).
student(mary).
student(bob).
teacher(smith).
teacher(jones).
course(math).
course(science).
course(history).

% Rules:
enrolled_in(Student, Course) :- 
    student(Student), 
    course(Course),
    member(Course, courses_taken(Student)).

courses_taken(john) := [math, science].
courses_taken(mary) := [math, history].
courses_taken(bob) := [science, history].

% Query the knowledge base.

?- enrolled_in(john, math).
% Yes

?- enrolled_in(mary, history).
% Yes

?- enrolled_in(bob, science).
% Yes

?- enrolled_in(alice, math).
% No
```

Explanation:

* The first three lines define facts about students, teachers, and courses.
* The fourth line defines a rule that states that a student is enrolled in a course if they are a student, the course is a course, and the course is in the list of courses that the student has taken.
* The fifth, sixth, and seventh lines define the list of courses that each student has taken.
* The last three lines are queries that ask the knowledge base whether or not a particular student is enrolled in a particular course.

This code demonstrates a simple Prolog program that uses facts and rules to represent knowledge about students, teachers, courses, and enrollment. The program can be used to answer queries about whether or not a particular student is enrolled in a particular course.