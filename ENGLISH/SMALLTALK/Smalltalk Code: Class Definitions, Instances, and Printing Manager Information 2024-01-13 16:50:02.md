```smalltalk
"Class Definition: Person"
Object subclass: Person [
    "Instance Variables"
    name: ''
    age: 0
    gender: ''
]

"Class Definition: Student"
Person subclass: Student [
    "Instance Variables"
    major: ''
    gpa: 0.0
]

"Class Definition: Employee"
Person subclass: Employee [
    "Instance Variables"
    company: ''
    salary: 0.0
    jobTitle: ''
]

"Class Definition: Manager"
Employee subclass: Manager [
    "Instance Variables"
    numEmployees: 0
]

"Instance Creation: Manager Object"
manager := Manager new.

"Setting Instance Variables for the Manager Object"
manager name: 'John Smith'.
manager age: 35.
manager gender: 'Male'.
manager company: 'Acme Corporation'.
manager salary: 100000.00.
manager jobTitle: 'CEO'.
manager numEmployees: 50.

"Printing Manager Information"
Transcript show: 'Manager Information:'.
Transcript cr.
Transcript show: 'Name: ', manager name.
Transcript cr.
Transcript show: 'Age: ', manager age.
Transcript cr.
Transcript show: 'Gender: ', manager gender.
Transcript cr.
Transcript show: 'Company: ', manager company.
Transcript cr.
Transcript show: 'Salary: ', manager salary.
Transcript cr.
Transcript show: 'Job Title: ', manager jobTitle.
Transcript cr.
Transcript show: 'Number of Employees: ', manager numEmployees.
Transcript cr.
```
Explanation:

1. **Class Definitions**:
   - `Person`: Defines the base class `Person` with instance variables for `name`, `age`, and `gender`.
   - `Student`: Subclass of `Person` representing students with additional instance variables for `major` and `gpa`.
   - `Employee`: Subclass of `Person` representing employees with instance variables for `company`, `salary`, and `jobTitle`.
   - `Manager`: Subclass of `Employee` representing managers with an additional instance variable for `numEmployees`.

2. **Instance Creation**:
   - `manager := Manager new.`: Creates a new instance of the `Manager` class and assigns it to the variable `manager`.

3. **Setting Instance Variables**:
   - We set the instance variables of the `manager` object with values for name, age, gender, company, salary, job title, and number of employees.

4. **Printing Manager Information**:
   - We use `Transcript` to print out the manager's information, including their name, age, gender, company, salary, job title, and number of employees.