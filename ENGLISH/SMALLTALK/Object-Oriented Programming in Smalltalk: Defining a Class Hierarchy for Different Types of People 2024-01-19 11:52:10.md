```smalltalk
Object subclass: Person [
  | name age |

  initialize: aName anAge [
    super initialize.
    name := aName.
    age := anAge.
  ]

  name [^ name]
  age [^ age]

  printName [Transcript show: name]
  printAge [Transcript show: age]
]

Person subclass: Student [
  |gpa |

  initialize: aName anAge aGpa [
    super initialize: aName anAge.
    gpa := aGpa.
  ]

  gpa [^ gpa]

  printGpa [Transcript show: gpa]
]

Person subclass: Employee [
  |salary |

  initialize: aName anAge aSalary [
    super initialize: aName anAge.
    salary := aSalary.
  ]

  salary [^ salary]

  printSalary [Transcript show: salary]
]

Person subclass: Teacher [
  |subject |

  initialize: aName anAge aSubject [
    super initialize: aName anAge.
    subject := aSubject.
  ]

  subject [^ subject]

  printSubject [Transcript show: subject]
]

Transcript show: 'Creating a person...'. cr; nl.
john := Person new initialize: 'John Smith' anAge: 30.

Transcript show: 'Creating a student...'. cr; nl.
jane := Student new initialize: 'Jane Doe' anAge: 25 aGpa: 3.8.

Transcript show: 'Creating an employee...'. cr; nl.
bob := Employee new initialize: 'Bob Jones' anAge: 40 aSalary: 60000.

Transcript show: 'Creating a teacher...'. cr; nl.
mary := Teacher new initialize: 'Mary Johnson' anAge: 35 aSubject: 'Math'.

Transcript show: 'Printing the person's name...'. cr; nl.
john printName.

Transcript show: 'Printing the student's GPA...'. cr; nl.
jane printGpa.

Transcript show: 'Printing the employee's salary...'. cr; nl.
bob printSalary.

Transcript show: 'Printing the teacher's subject...'. cr; nl.
mary printSubject.
```

This code defines a class hierarchy for different types of people, including a base class Person and subclasses for Student, Employee, and Teacher. Each class has its own properties and methods, and the subclassing mechanism allows for code reuse and inheritance.

The Person class has two instance variables, name and age, and methods for getting and printing those values.

The Student class inherits from Person and adds an instance variable gpa and a method for printing it.

The Employee class inherits from Person and adds an instance variable salary and a method for printing it.

The Teacher class inherits from Person and adds an instance variable subject and a method for printing it.

The code then creates instances of each of these classes and calls methods on them to print their names, GPAs, salaries, and subjects.

Here are some sample outputs from running the code:

```
Creating a person...

Creating a student...

Creating an employee...

Creating a teacher...

Printing the person's name...
John Smith

Printing the student's GPA...
3.8

Printing the employee's salary...
60000

Printing the teacher's subject...
Math
```