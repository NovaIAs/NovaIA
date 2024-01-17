```smalltalk
Object subclass: Employee [
    "Instance variables for the employee's name, salary, and department."
    name: String.
    salary: Money.
    department: String.

    "Constructor method to create a new employee with a name, salary, and department."
    initialize: aName aSalary aDepartment [
        super initialize.
        name := aName.
        salary := aSalary.
        department := aDepartment.
    ]

    "Accessor methods to get the employee's name, salary, and department."
    getName [ ^name ]
    getSalary [ ^salary ]
    getDepartment [ ^department ]

    "Method to print the employee's information."
    printOn: aStream [
        aStream nextPutAll: 'Employee: '; nextPutAll: name; nextPutAll: ', ';
        nextPutAll: 'Salary: '; nextPutAll: salary printString; nextPutAll: ', ';
        nextPutAll: 'Department: '; nextPutAll: department; cr
    ]
]

Employee subclass: Manager [
    "Instance variable for the manager's subordinates."
    subordinates: OrderedCollection.

    "Constructor method to create a new manager with a name, salary, department, and subordinates."
    initialize: aName aSalary aDepartment someSubordinates [
        super initialize: aName aSalary aDepartment.
        subordinates := someSubordinates.
    ]

    "Accessor method to get the manager's subordinates."
    getSubordinates [ ^subordinates ]

    "Method to add a subordinate to the manager's list of subordinates."
    addSubordinate: anEmployee [
        subordinates add: anEmployee
    ]

    "Method to print the manager's information, including their subordinates."
    printOn: aStream [
        super printOn: aStream.
        aStream nextPutAll: 'Subordinates: '; nextPutAll: subordinates printString; cr
    ]
]

Manager subclass: SalesManager [
    "Instance variable for the sales manager's sales target."
    salesTarget: Money.

    "Constructor method to create a new sales manager with a name, salary, department, subordinates, and sales target."
    initialize: aName aSalary aDepartment someSubordinates aSalesTarget [
        super initialize: aName aSalary aDepartment someSubordinates.
        salesTarget := aSalesTarget.
    ]

    "Accessor method to get the sales manager's sales target."
    getSalesTarget [ ^salesTarget ]

    "Method to print the sales manager's information, including their sales target."
    printOn: aStream [
        super printOn: aStream.
        aStream nextPutAll: 'Sales Target: '; nextPutAll: salesTarget printString; cr
    ]
]

"Create a few employees, including managers and sales managers, and add them to a collection."
employees := OrderedCollection new.
employees add: (Employee new initialize: 'John Smith' aSalary: (Money new initialize: 50000) aDepartment: 'Engineering').
employees add: (Manager new initialize: 'Mary Johnson' aSalary: (Money new initialize: 75000) aDepartment: 'Sales' someSubordinates: OrderedCollection new).
employees add: (SalesManager new initialize: 'Bob Brown' aSalary: (Money new initialize: 100000) aDepartment: 'Sales' someSubordinates: OrderedCollection new aSalesTarget: (Money new initialize: 1000000)).

"Print the information for each employee in the collection."
employees do: [:employee | employee printOn: Transcript].
```

This code defines a class hierarchy for employees, including managers and sales managers, and demonstrates object-oriented programming principles in Smalltalk. Here's a detailed explanation:

- The `Object` class is the base class for all objects in Smalltalk.

- The `Employee` class is a subclass of `Object` and represents a generic employee with attributes like name, salary, and department.

- The `Manager` class is a subclass of `Employee` and represents a manager with additional attributes like subordinates.

- The `SalesManager` class is a subclass of `Manager` and represents a sales manager with an additional attribute, sales target.

- The `initialize` method in each class is used to initialize the object with appropriate values.

- Accessor methods like `getName`, `getSalary`, `getDepartment`, `getSubordinates`, and `getSalesTarget` are used to retrieve the values of the corresponding attributes.

- The `printOn:` method in each class is used to print the object's information.

- The `employees` collection is created and populated with instances of `Employee`, `Manager`, and `SalesManager` classes.

- The `do:` method is used to iterate through the `employees` collection and call the `printOn:` method on each employee to print their information.

This code demonstrates the use of classes, inheritance, polymorphism, and object-oriented programming concepts in Smalltalk. It's a comprehensive example that showcases the language's features and capabilities.