```php
<?php

// Define a class called User
class User {

    // Define the properties of the User class
    private $id;
    private $username;
    private $email;
    private $password;

    // Define the constructor method of the User class
    public function __construct($id, $username, $email, $password) {
        $this->id = $id;
        $this->username = $username;
        $this->email = $email;
        $this->password = $password;
    }

    // Define the getter methods of the User class
    public function getId() {
        return $this->id;
    }

    public function getUsername() {
        return $this->username;
    }

    public function getEmail() {
        return $this->email;
    }

    public function getPassword() {
        return $this->password;
    }

    // Define the setter methods of the User class
    public function setId($id) {
        $this->id = $id;
    }

    public function setUsername($username) {
        $this->username = $username;
    }

    public function setEmail($email) {
        $this->email = $email;
    }

    public function setPassword($password) {
        $this->password = $password;
    }

}

// Define a class called UserRepository
class UserRepository {

    // Define the method to create a new user
    public function create(User $user) {
        // Insert the user data into the database
    }

    // Define the method to find a user by their ID
    public function findById($id) {
        // Select the user data from the database by their ID
    }

    // Define the method to find a user by their username
    public function findByUsername($username) {
        // Select the user data from the database by their username
    }

    // Define the method to find a user by their email
    public function findByEmail($email) {
        // Select the user data from the database by their email
    }

    // Define the method to update a user
    public function update(User $user) {
        // Update the user data in the database
    }

    // Define the method to delete a user
    public function delete(User $user) {
        // Delete the user data from the database
    }

}

// Define a class called UserService
class UserService {

    // Define the property to store the UserRepository object
    private $userRepository;

    // Define the constructor method of the UserService class
    public function __construct(UserRepository $userRepository) {
        $this->userRepository = $userRepository;
    }

    // Define the method to create a new user
    public function create(User $user) {
        // Call the create method of the UserRepository to create a new user
        $this->userRepository->create($user);
    }

    // Define the method to find a user by their ID
    public function findById($id) {
        // Call the findById method of the UserRepository to find a user by their ID
        $user = $this->userRepository->findById($id);

        // Return the user object
        return $user;
    }

    // Define the method to find a user by their username
    public function findByUsername($username) {
        // Call the findByUsername method of the UserRepository to find a user by their username
        $user = $this->userRepository->findByUsername($username);

        // Return the user object
        return $user;
    }

    // Define the method to find a user by their email
    public function findByEmail($email) {
        // Call the findByEmail method of the UserRepository to find a user by their email
        $user = $this->userRepository->findByEmail($email);

        // Return the user object
        return $user;
    }

    // Define the method to update a user
    public function update(User $user) {
        // Call the update method of the UserRepository to update a user
        $this->userRepository->update($user);
    }

    // Define the method to delete a user
    public function delete(User $user) {
        // Call the delete method of the UserRepository to delete a user
        $this->userRepository->delete($user);
    }

}

// Create a new UserRepository object
$userRepository = new UserRepository();

// Create a new UserService object
$userService = new UserService($userRepository);

// Create a new User object
$user = new User(1, 'johnDoe', 'john.doe@example.com', 'password123');

// Call the create method of the UserService to create a new user
$userService->create($user);

// Call the findById method of the UserService to find a user by their ID
$user = $userService->findById(1);

// Call the findByUsername method of the UserService to find a user by their username
$user = $userService->findByUsername('johnDoe');

// Call the findByEmail method of the UserService to find a user by their email
$user = $userService->findByEmail('john.doe@example.com');

// Call the update method of the UserService to update a user
$user->setUsername('newUsername');
$userService->update($user);

// Call the delete method of the UserService to delete a user
$userService->delete($user);

?>
```

Explanation:

This code is a complex and differentiated PHP script that demonstrates the implementation of a user management system using object-oriented programming principles. Here's a detailed explanation of the code:

**Classes and Objects:**

1. **User Class:** The User class defines the data structure and behavior for representing a user. It has private properties for user ID, username, email, and password. It also has getter and setter methods for accessing and modifying these properties.

2. **UserRepository Class:** The UserRepository class is responsible for performing CRUD (Create, Read, Update, Delete) operations on User objects. It defines methods for creating, finding, updating, and deleting users in a database or any persistent storage system.

3. **UserService Class:** The UserService class handles the business logic related to user management. It has a property to store a UserRepository object, which is passed to its constructor. The UserService class provides methods for creating, finding, updating, and deleting users using the UserRepository object.

**Usage of the Classes:**

1. An instance of the UserRepository class is created.

2. An instance of the UserService class is created, passing the UserRepository instance as an argument.

3. A new User object is created and its properties are set.

4. The create method of the UserService is called to save the new user in the database using the UserRepository.

5. The findById, findByUsername, and findByEmail methods of the UserService are called to retrieve user information based on different criteria using the UserRepository.

6. The update method of the UserService is called to update the username of the user.

7. The delete method of the UserService is called to delete the user from the database.

This script demonstrates the implementation of a user management system using object-oriented programming, separation of concerns, and dependency injection through constructor injection. It showcases the use of classes, objects, methods, and properties to model real-world entities and perform operations on them.