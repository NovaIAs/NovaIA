```smalltalk

// Define a class called "Person"
Class Person [

    // Instance variables (attributes)
    name: String;
    age: Integer;
    address: String;

    // Constructor (initializer)
    initialize [
        "Constructs a new Person object."

        // Initialize instance variables
        name := 'John Doe';
        age := 20;
        address := '123 Main Street';
    ]

    // Instance methods (behaviors)
    sayHello [
        "Prints a greeting message."

        Transcript show: "Hello, my name is ", name, '!', newline.
    ]
    getName [
        "Returns the name of the person."

        ^name;
    ]
    setAge [ newAge: Integer ] [
        "Sets the age of the person."

        age := newAge;
    ]
    getAddress [
        "Returns the address of the person."

        ^address;
    ]
];

// Create an instance of the Person class
person1 := Person new;

// Set the name, age, and address of the person
person1 name: 'Jane Smith';
person1 setAge: 25;
person1 address: '456 Elm Street';

// Print a greeting message
person1 sayHello;

// Print the name, age, and address of the person
Transcript show: 'Name: ', person1 getName, newline;
Transcript show: 'Age: ', person1 age, newline;
Transcript show: 'Address: ', person1 getAddress, newline;
```

Explanation:

1. **Defining the Person Class:**
   - `Class Person [` starts the definition of the `Person` class.
   - `name: String;` declares an instance variable `name` to store the person's name.
   - `age: Integer;` declares an instance variable `age` to store the person's age.
   - `address: String;` declares an instance variable `address` to store the person's address.

2. **Constructor:**
   - `initialize [` starts the definition of the constructor (initializer).
   - `name := 'John Doe';` initializes the `name` instance variable to 'John Doe'.
   - `age := 20;` initializes the `age` instance variable to 20.
   - `address := '123 Main Street';` initializes the `address` instance variable to '123 Main Street'.

3. **Instance Methods:**
   - `sayHello [` defines an instance method `sayHello` that prints a greeting message.
   - `getName [` defines an instance method `getName` that returns the person's name.
   - `setAge [ newAge: Integer ] [` defines an instance method `setAge` that sets the person's age.
   - `getAddress [` defines an instance method `getAddress` that returns the person's address.

4. **Creating an Instance of the Person Class:**
   - `person1 := Person new;` creates a new instance of the `Person` class and assigns it to the variable `person1`.

5. **Setting Person's Attributes:**
   - `person1 name: 'Jane Smith';` sets the `name` attribute of `person1` to 'Jane Smith'.
   - `person1 setAge: 25;` sets the `age` attribute of `person1` to 25.
   - `person1 address: '456 Elm Street';` sets the `address` attribute of `person1` to '456 Elm Street'.

6. **Invoking Instance Methods:**
   - `person1 sayHello;` invokes the `sayHello` method on `person1`, which prints a greeting message.
   - `Transcript show: 'Name: ', person1 getName, newline;` prints the person's name to the transcript.
   - `Transcript show: 'Age: ', person1 age, newline;` prints the person's age to the transcript.
   - `Transcript show: 'Address: ', person1 getAddress, newline;` prints the person's address to the transcript.

This code demonstrates object-oriented programming principles in Smalltalk by defining a `Person` class with attributes (instance variables), constructor (initializer), and instance methods (behaviors). It also shows how to create an instance of the class, set and get attributes, and invoke instance methods.