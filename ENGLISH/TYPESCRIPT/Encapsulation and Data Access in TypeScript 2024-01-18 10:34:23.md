```typescript
// Define a TypeScript class to represent a complex object
class ComplexObject {
  // Declare private fields to hold data
  private _firstName: string;
  private _lastName: string;
  private _age: number;
  private _address: string;
  private _phoneNumber: string;

  // Declare a constructor to initialize the object
  constructor(firstName: string, lastName: string, age: number, address: string, phoneNumber: string) {
    this._firstName = firstName;
    this._lastName = lastName;
    this._age = age;
    this._address = address;
    this._phoneNumber = phoneNumber;
  }

  // Define getter and setter methods to access and modify private fields
  get firstName(): string {
    return this._firstName;
  }

  set firstName(value: string) {
    this._firstName = value;
  }

  get lastName(): string {
    return this._lastName;
  }

  set lastName(value: string) {
    this._lastName = value;
  }

  get age(): number {
    return this._age;
  }

  set age(value: number) {
    this._age = value;
  }

  get address(): string {
    return this._address;
  }

  set address(value: string) {
    this._address = value;
  }

  get phoneNumber(): string {
    return this._phoneNumber;
  }

  set phoneNumber(value: string) {
    this._phoneNumber = value;
  }

  // Define a method to print the object's details
  printDetails(): void {
    console.log(`First Name: ${this._firstName}`);
    console.log(`Last Name: ${this._lastName}`);
    console.log(`Age: ${this._age}`);
    console.log(`Address: ${this._address}`);
    console.log(`Phone Number: ${this._phoneNumber}`);
  }
}

// Create an instance of the ComplexObject class
const person1 = new ComplexObject("John", "Doe", 30, "123 Main Street", "555-123-4567");

// Print the object's details
person1.printDetails();
```

Explanation:

This TypeScript code defines a complex class named "ComplexObject" that represents a complex object with various properties and methods. The class has private fields to encapsulate data and getter and setter methods to access and modify these fields. It also includes a method to print the object's details. An instance of the "ComplexObject" class is created and its details are printed to the console.

Here's a detailed breakdown of the code:

1. **Class Definition**:
   - We define a TypeScript class named "ComplexObject".

2. **Private Fields**:
   - Inside the class, we declare private fields using the `private` keyword. These fields are used to hold data for the object. In this case, we have private fields for `firstName`, `lastName`, `age`, `address`, and `phoneNumber`.

3. **Constructor**:
   - We define a constructor for the class that takes parameters for each of the private fields. This constructor is responsible for initializing the object with the provided values.

4. **Getter and Setter Methods**:
   - For each private field, we define getter and setter methods. These methods allow us to access and modify the private fields from outside the class while maintaining encapsulation.

5. **`printDetails()` Method**:
   - We define a method called `printDetails()` that prints the details of the object, including its first name, last name, age, address, and phone number.

6. **Creating an Instance**:
   - We create an instance of the "ComplexObject" class called `person1` and provide values for its properties.

7. **Calling the `printDetails()` Method**:
   - We call the `printDetails()` method on the `person1` object to print its details to the console.

When you run this code, it will create a complex object representing a person with the provided details and then print those details to the console.