```typescript
// 1. Declare an interface for a type of vehicle
interface Vehicle {
  make: string;
  model: string;
  year: number;
  color: string;
  isElectric: boolean;
  start(): void;
  stop(): void;
}

// 2. Define a function to create a new vehicle object based on the interface
const createVehicle = (make: string, model: string, year: number, color: string, isElectric: boolean): Vehicle => {
  return {
    make,
    model,
    year,
    color,
    isElectric,
    start: () => { console.log('Vehicle started'); },
    stop: () => { console.log('Vehicle stopped'); },
  };
};

// 3. Instantiate a new vehicle object using the function
const myVehicle = createVehicle('Tesla', 'Model S', 2022, 'Black', true);

// 4. Call the start() and stop() methods on the vehicle object
myVehicle.start();
myVehicle.stop();

// 5. Define a function to calculate the total cost of a purchase based on the price and tax rate
const calculateTotalCost = (price: number, taxRate: number): number => {
  return price * (1 + taxRate);
};

// 6. Invoke the calculateTotalCost() function and store the result in a variable
const totalCost = calculateTotalCost(100, 0.08);

// 7. Create a class representing a customer
class Customer {
  name: string;
  email: string;
  address: string;

  constructor(name: string, email: string, address: string) {
    this.name = name;
    this.email = email;
    this.address = address;
  }
}

// 8. Instantiate a new customer object using the class
const customer = new Customer('John Doe', 'john.doe@example.com', '123 Main Street');

// 9. Use a third-party library to send an email to the customer
// This is a third party library, like SendGrid, Mailgun, or Nodemailer.
const sendEmail = (to: string, subject: string, body: string): void => {
  // This is a library function to send an email.
  // Actual implementation would involve using a library to send the email.
  console.log(`Sending email to ${to} with subject "${subject}" and body "${body}"`);
};

// 10. Send an email to the customer
sendEmail(customer.email, 'Welcome!', 'Thank you for signing up!');
```

**Explanation:**

1. We define an interface called `Vehicle` that specifies the properties and methods required for any type of vehicle.

2. We define a function `createVehicle` that takes various parameters and returns a new vehicle object based on the `Vehicle` interface.

3. We create a new vehicle object using the `createVehicle` function and store it in the `myVehicle` variable.

4. We call the `start()` and `stop()` methods on the `myVehicle` object to simulate starting and stopping the vehicle.

5. We define a function `calculateTotalCost` that calculates the total cost of a purchase by adding the price and the tax amount.

6. We invoke the `calculateTotalCost` function and store the result in the `totalCost` variable.

7. We define a class `Customer` representing a customer with properties like name, email, and address.

8. We create a new `Customer` object using the `new` keyword and store it in the `customer` variable.

9. We use a third-party library (e.g., `sendEmail`) to send an email to the customer using their email address.

10. We call the `sendEmail` function to send a welcome email to the customer.

This code demonstrates various concepts in TypeScript, including interfaces, functions, classes, third-party library usage, and object-oriented programming. It's a comprehensive example that covers multiple aspects of the language.