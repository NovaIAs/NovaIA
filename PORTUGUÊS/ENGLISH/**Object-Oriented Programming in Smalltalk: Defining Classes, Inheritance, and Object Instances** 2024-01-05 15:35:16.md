```smalltalk

// Define a class called "Person" with attributes "name" and "age"
class Person {
    attributes: name, age;

    // Constructor method to initialize the attributes
    initialize: aName anAge {
        name := aName;
        age := anAge;
    }

    // Method to print the person's name and age
    print {
        Transcript show: 'Name: ', name;
        Transcript show: ' Age: ', age, cr;
    }
}

// Define a class called "Employee" which inherits from "Person" and adds an attribute "salary"
class Employee extends: Person {
    attributes: salary;

    // Constructor method to initialize the attributes
    initialize: aName anAge aSalary {
        super initialize: aName anAge;
        salary := aSalary;
    }

    // Method to print the employee's name, age, and salary
    print {
        super print;
        Transcript show: ' Salary: ', salary, cr;
    }
}

// Define a class called "Manager" which inherits from "Employee" and adds an attribute "department"
class Manager extends: Employee {
    attributes: department;

    // Constructor method to initialize the attributes
    initialize: aName anAge aSalary aDepartment {
        super initialize: aName anAge aSalary;
        department := aDepartment;
    }

    // Method to print the manager's name, age, salary, and department
    print {
        super print;
        Transcript show: ' Department: ', department, cr;
    }
}

// Create an instance of the "Person" class
person := Person new initialize: 'John Doe' 30.

// Create an instance of the "Employee" class
employee := Employee new initialize: 'Jane Smith' 40 50000.

// Create an instance of the "Manager" class
manager := Manager new initialize: 'Michael Jones' 50 100000 'Engineering'.

// Print the details of each object
person print;
employee print;
manager print;

```

**Explanation:**

1. We define three classes: "Person", "Employee", and "Manager".

2. The "Person" class has attributes "name" and "age". It has a constructor method to initialize these attributes and a method "print" to display the person's name and age.

3. The "Employee" class inherits from the "Person" class and adds an attribute "salary". It has a constructor method to initialize all three attributes and a method "print" to display the employee's name, age, and salary.

4. The "Manager" class inherits from the "Employee" class and adds an attribute "department". It has a constructor method to initialize all four attributes and a method "print" to display the manager's name, age, salary, and department.

5. We create instances of each class and assign them to variables.

6. Finally, we call the "print" method on each object to display their details.