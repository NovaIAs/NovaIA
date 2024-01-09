```smalltalk
Object subclass: #Employee [#name #salary]
    instanceVariableNames: 'name salary'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Payroll'

Employee class methods
    [
    new name: aName salary: aSalary
        "Create a new Employee instance with the given name and salary."

        ^ super new
            name: aName;
            salary: aSalary
    ]

Employee instance methods
    [
    raiseSalaryBy: aPercentage
        "Raise the employee's salary by the given percentage."

        salary := salary * (1 + aPercentage / 100)
    ]

Manager subclass: #Manager [#employees]
    instanceVariableNames: 'employees'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Payroll'

Manager class methods
    [
    new name: aName salary: aSalary employees: someEmployees
        "Create a new Manager instance with the given name, salary, and employees."

        ^ super new
            name: aName;
            salary: aSalary;
            employees: someEmployees
    ]

Manager instance methods
    [
    totalSalary
        "Return the total salary of the manager and their employees."

        ^ salary + employees collect: [:employee | employee salary] inject: 0 into: [:sum | :salary | sum + salary]
    ]

Company subclass: #Company [#employees]
    instanceVariableNames: 'employees'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Payroll'

Company class methods
    [
    new name: aName employees: someEmployees
        "Create a new Company instance with the given name and employees."

        ^ super new
            name: aName;
            employees: someEmployees
    ]

Company instance methods
    [
    totalSalary
        "Return the total salary of all the employees in the company."

        ^ employees collect: [:employee | employee salary] inject: 0 into: [:sum | :salary | sum + salary]
    ]

Employee subclass: #HourlyEmployee [#hoursWorked]
    instanceVariableNames: 'hoursWorked'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Payroll'

HourlyEmployee class methods
    [
    new name: aName salary: aSalary hoursWorked: someHours
        "Create a new HourlyEmployee instance with the given name, salary, and hours worked."

        ^ super new
            name: aName;
            salary: aSalary;
            hoursWorked: someHours
    ]

HourlyEmployee instance methods
    [
    raiseSalaryBy: aPercentage
        "Raise the employee's salary by the given percentage."

        salary := salary * (1 + aPercentage / 100)
    ]

HourlyEmployee instance methods
    [
    totalSalary
        "Return the total salary of the hourly employee."

        ^ salary * hoursWorked
    ]

Company subclass: #RetailCompany [#customers]
    instanceVariableNames: 'customers'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Retail'

RetailCompany class methods
    [
    new name: aName employees: someEmployees customers: someCustomers
        "Create a new RetailCompany instance with the given name, employees, and customers."

        ^ super new
            name: aName;
            employees: someEmployees;
            customers: someCustomers
    ]

RetailCompany instance methods
    [
    totalRevenue
        "Return the total revenue of the retail company."

        ^ customers collect: [:customer | customer purchases] inject: 0 into: [:sum | :purchases | sum + purchases]
    ]
```

This code defines a class hierarchy for modeling employees and companies in a payroll system. The `Employee` class represents an individual employee, with properties for their name, salary, and a method to raise their salary by a given percentage. The `Manager` class extends `Employee` and adds a property for the manager's employees, as well as a method to calculate the total salary of the manager and their employees.

The `Company` class represents a company, with a property for its employees and a method to calculate the total salary of all the employees in the company. The `HourlyEmployee` class extends `Employee` and represents an employee who is paid by the hour, with properties for their hourly rate and hours worked, as well as a method to calculate their total salary.

Finally, the `RetailCompany` class extends `Company` and represents a retail company, with a property for its customers and a method to calculate the total revenue of the company.

This code demonstrates the use of class hierarchies, properties, methods, and inheritance in Smalltalk.