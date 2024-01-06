```typescript
interface User {
  id: number;
  name: string;
  email: string;
  age: number;
  address: Address;
}

interface Address {
  street: string;
  city: string;
  state: string;
  zipCode: string;
}

type UserWithRole = User & { role: string };

const users: User[] = [
  {
    id: 1,
    name: 'John Doe',
    email: 'johndoe@example.com',
    age: 30,
    address: {
      street: '123 Main Street',
      city: 'Anytown',
      state: 'CA',
      zipCode: '12345'
    }
  },
  {
    id: 2,
    name: 'Jane Smith',
    email: 'janesmith@example.com',
    age: 25,
    address: {
      street: '456 Elm Street',
      city: 'Anytown',
      state: 'CA',
      zipCode: '23456'
    }
  },
  {
    id: 3,
    name: 'Bill Jones',
    email: 'billjones@example.com',
    age: 40,
    address: {
      street: '789 Oak Street',
      city: 'Anytown',
      state: 'CA',
      zipCode: '34567'
    }
  }
];

// Create a new user
const newUser: User = {
  id: 4,
  name: 'New User',
  email: 'newuser@example.com',
  age: 20,
  address: {
    street: '1010 Pine Street',
    city: 'Anytown',
    state: 'CA',
    zipCode: '45678'
  }
};

users.push(newUser);

// Find a user by ID
const findUserById = (id: number): User | undefined => {
  return users.find((user) => user.id === id);
};

// Find all users with a specific role
const findUsersWithRole = (role: string): UserWithRole[] => {
  return users.filter((user) => user.role === role);
};

// Update a user's information
const updateUser = (user: User): void => {
  const index = users.findIndex((u) => u.id === user.id);
  if (index !== -1) {
    users[index] = user;
  }
};

// Delete a user by ID
const deleteUser = (id: number): void => {
  const index = users.findIndex((user) => user.id === id);
  if (index !== -1) {
    users.splice(index, 1);
  }
};

// Print all users to the console
const printUsers = (): void => {
  console.log(users);
};

// Usage
const user1 = findUserById(1);
console.log(user1); // { id: 1, name: 'John Doe', email: 'johndoe@example.com', age: 30, address: { ... } }

const usersWithAdminRole = findUsersWithRole('admin');
console.log(usersWithAdminRole); // []

updateUser({ id: 2, name: 'Jane Smith', email: 'janesmith@example.com', age: 25, address: { ... } });
console.log(users); // [ { id: 1, name: 'John Doe', ... }, { id: 2, name: 'Jane Smith', ... }, { id: 3, name: 'Bill Jones', ... } ]

deleteUser(3);
console.log(users); // [ { id: 1, name: 'John Doe', ... }, { id: 2, name: 'Jane Smith', ... } ]

printUsers();
// [
//   { id: 1, name: 'John Doe', email: 'johndoe@example.com', age: 30, address: { ... } },
//   { id: 2, name: 'Jane Smith', email: 'janesmith@example.com', age: 25, address: { ... } }
// ]
```

This code defines a TypeScript interface for representing user data, including their ID, name, email, age, and address. It also defines a type alias for users with an additional `role` property.

The code then creates an array of user objects and adds a new user to the array. It also defines functions for finding users by ID, finding users with a specific role, updating user information, deleting users by ID, and printing all users to the console.

Finally, the code demonstrates how to use these functions to perform various operations on the array of users.