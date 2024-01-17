```cool
module bar {
    class Person {
        name: String;
        age: Int;

        constructor(name: String, age: Int) {
            this.name := name;
            this.age := age;
        }

        say_name() {
            print_string("My name is ");
            print_string(this.name);
            print_newline();
        }

        say_age() {
            print_string("My age is ");
            print_int(this.age);
            print_newline();
        }
    }

    class Student extends Person {
        grade: Int;

        constructor(name: String, age: Int, grade: Int) {
            super(name, age);
            this.grade := grade;
        }

        say_grade() {
            print_string("My grade is ");
            print_int(this.grade);
            print_newline();
        }
    }

    class Teacher extends Person {
        subject: String;

        constructor(name: String, age: Int, subject: String) {
            super(name, age);
            this.subject := subject;
        }

        say_subject() {
            print_string("My subject is ");
            print_string(this.subject);
            print_newline();
        }
    }

    class Main {
        main() {
            let student: Student := new Student("John", 20, 3);
            let teacher: Teacher := new Teacher("Mary", 30, "Mathematics");

            student.say_name();
            student.say_age();
            student.say_grade();

            print_newline();

            teacher.say_name();
            teacher.say_age();
            teacher.say_subject();
        }
    }
}
```

This code defines a module named `bar` that contains four classes: `Person`, `Student`, `Teacher`, and `Main`.

The `Person` class is a base class that defines two attributes: `name` and `age`. It also defines two methods: `say_name()` and `say_age()`, which print the person's name and age, respectively.

The `Student` class extends the `Person` class and adds a new attribute called `grade`. It also defines a new method called `say_grade()`, which prints the student's grade.

The `Teacher` class also extends the `Person` class and adds a new attribute called `subject`. It also defines a new method called `say_subject()`, which prints the teacher's subject.

The `Main` class contains the `main()` method, which is the entry point of the program. In the `main()` method, two instances of the `Student` and `Teacher` classes are created and their respective methods are called to print their names, ages, grades, and subjects.

Here's a step-by-step explanation of the code:

1. The `module bar` statement defines a module named `bar`. All the classes and methods defined within this module will belong to this module.

2. The `class Person` statement defines a class named `Person`. This class has two attributes: `name` and `age`, and two methods: `say_name()` and `say_age()`.

3. The `constructor` method is a special method that is called when an instance of a class is created. In this case, the `Person` class's constructor takes two arguments: `name` and `age`. It assigns these values to the `name` and `age` attributes of the newly created object.

4. The `say_name()` and `say_age()` methods are instance methods. This means that they can only be called on an instance of the `Person` class. These methods print the person's name and age, respectively.

5. The `class Student` statement defines a class named `Student`. This class extends the `Person` class, which means that it inherits all the attributes and methods of the `Person` class. The `Student` class also defines a new attribute called `grade`.

6. The `constructor` method of the `Student` class takes three arguments: `name`, `age`, and `grade`. It calls the `Person` class's constructor to initialize the `name` and `age` attributes, and then it assigns the `grade` argument to the `grade` attribute.

7. The `say_grade()` method is an instance method of the `Student` class. It prints the student's grade.

8. The `class Teacher` statement defines a class named `Teacher`. This class also extends the `Person` class and defines a new attribute called `subject`.

9. The `constructor` method of the `Teacher` class takes three arguments: `name`, `age`, and `subject`. It calls the `Person` class's constructor to initialize the `name` and `age` attributes, and then it assigns the `subject` argument to the `subject` attribute.

10. The `say_subject()` method is an instance method of the `Teacher` class. It prints the teacher's subject.

11. The `class Main` statement defines a class named `Main`. This class contains the `main()` method, which is the entry point of the program.

12. The `main()` method creates two instances of the `Student` and `Teacher` classes. It then calls the `say_name()`, `say_age()`, `say_grade()`, and `say_subject()` methods on these instances to print their respective names, ages, grades, and subjects.

When you run this program, it will output the following:

```
My name is John
My age is 20
My grade is 3

My name is Mary
My age is 30
My subject is Mathematics
```