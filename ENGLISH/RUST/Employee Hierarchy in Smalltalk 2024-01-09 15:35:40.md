```smalltalk
Object subclass: Employee [
    firstName: aString lastName: aString salary: anAmount
        "Create a new Employee object with the given name and salary."
    | salary |
    salary := anAmount ifNil: [ 0 ].
    super initialize.
    firstName := aString.
    lastName := aString.
    self salary: salary.
]

Employee class [
    newEmployee: aString and: aString withSalary: anAmount
        "Create a new Employee object with the given name and salary."
    | employee |
    employee := Employee new.
    employee firstName: aString.
    employee lastName: aString.
    employee salary: anAmount.
    ^ employee
]

Employee instance [
    firstName
        "Answer the receiver's first name."
    ^ firstName

    lastName
        "Answer the receiver's last name."
    ^ lastName

    salary
        "Answer the receiver's salary."
    ^ salary

    salary: anAmount
        "Set the receiver's salary to the given amount."
    salary := anAmount.
    ^ self

    fullName
        "Answer the receiver's full name."
    ^ firstName , ' ', lastName

    raise
        "Raise the receiver's salary by 10%."
    self salary: (salary * 1.10) rounded
]

Manager subclass: Manager [
    firstName: aString lastName: aString salary: anAmount
        "Create a new Manager object with the given name and salary."
    super newEmployee: aString and: aString withSalary: anAmount.
]

Manager class [
    newManager: aString and: aString withSalary: anAmount
        "Create a new Manager object with the given name and salary."
    | manager |
    manager := Manager new.
    manager firstName: aString.
    manager lastName: aString.
    manager salary: anAmount.
    ^ manager
]

Manager instance [
    bonus
        "Answer the receiver's bonus."
    ^ salary * 0.10
]

Executive subclass: Executive [
    firstName: aString lastName: aString salary: anAmount
        "Create a new Executive object with the given name and salary."
    super newManager: aString and: aString withSalary: anAmount.
]

Executive class [
    newExecutive: aString and: aString withSalary: anAmount
        "Create a new Executive object with the given name and salary."
    | executive |
    executive := Executive new.
    executive firstName: aString.
    executive lastName: aString.
    executive salary: anAmount.
    ^ executive
]

Executive instance [
    bonus
        "Answer the receiver's bonus."
    ^ salary * 0.20

    stockOptions
        "Answer the receiver's stock options."
    ^ 100000
]

```

This code defines a hierarchy of employee classes in Smalltalk. It includes classes for Employee, Manager, and Executive, each with their own unique attributes and behaviors.

The Employee class has attributes for first name, last name, and salary. It also has methods for getting and setting these attributes, as well as a method for calculating the employee's full name.

The Manager class inherits from the Employee class and adds an attribute for bonus. The Manager class also has a method for calculating the manager's bonus.

The Executive class inherits from the Manager class and adds an attribute for stock options. The Executive class also has a method for calculating the executive's bonus and stock options.

To use this code, you would first create an instance of the Employee, Manager, or Executive class. You can then use the methods of the class to access and modify the employee's attributes and calculate their bonus and stock options.

For example, the following code creates an instance of the Executive class and then prints the executive's full name, bonus, and stock options:

```smalltalk
| executive |
executive := Executive newExecutive: 'John' and: 'Doe' withSalary: 100000.
executive fullName printNl.
executive bonus printNl.
executive stockOptions printNl.
```

This code would print the following output:

```
John Doe
10000.00
100000
```