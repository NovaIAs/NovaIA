```swift
// Libraries
import UIKit

// Structs
struct User {
    var username: String
    var password: String
    var email: String
}

// Enums
enum AuthError: Error {
    case invalidCredentials
    case userNotFound
    case emailAlreadyExists
}

// Classes
class AuthenticationManager {
    // Singleton instance
    static let shared = AuthenticationManager()
    
    // Private initializer to prevent direct instantiation
    private init() {}
    
    // Method to register a new user
    func registerUser(username: String, password: String, email: String) throws {
        // Check if the email already exists
        if !isEmailAvailable(email: email) {
            throw AuthError.emailAlreadyExists
        }
        
        // Create a new user object
        let user = User(username: username, password: password, email: email)
        
        // Save the user to the database
        try saveUser(user: user)
    }
    
    // Method to login a user
    func loginUser(username: String, password: String) throws -> User {
        // Check if the user exists
        guard let user = findUser(username: username) else {
            throw AuthError.userNotFound
        }
        
        // Check if the password is correct
        guard user.password == password else {
            throw AuthError.invalidCredentials
        }
        
        // Return the user object
        return user
    }
    
    // Private methods
    private func isEmailAvailable(email: String) -> Bool {
        // Check the database to see if the email already exists
        return true
    }
    
    private func saveUser(user: User) throws {
        // Save the user object to the database
        // This method could potentially throw an error
        // if the database is unavailable or there is a network issue
    }
    
    private func findUser(username: String) -> User? {
        // Search the database for a user with the given username
        // This method could potentially return nil if the user is not found
        return nil
    }
}

// Usage
// Register a new user
do {
    try AuthenticationManager.shared.registerUser(username: "johndoe", password: "password", email: "johndoe@example.com")
} catch let error as AuthError {
    print("Registration failed with error: \(error)")
}

// Login a user
do {
    let user = try AuthenticationManager.shared.loginUser(username: "johndoe", password: "password")
    print("User \(user.username) logged in successfully")
} catch let error as AuthError {
    print("Login failed with error: \(error)")
}
```

Explanation:

1. Libraries: We import the UIKit library, which is necessary for working with the UI in iOS apps.

2. Structs: We define a `User` struct to represent user data with properties like username, password, and email.

3. Enums: We define an `AuthError` enum to represent different types of authentication errors that can occur, such as invalid credentials, user not found, etc.

4. Classes:
   - `AuthenticationManager`: This is the main class that handles user authentication.
   - It has a shared singleton instance to ensure that there is only one instance of this class in the app.
   - It has methods for registering a new user, logging in a user, and checking if the email is available.

5. Usage: We demonstrate how to use the `AuthenticationManager` class to register a new user and log in a user.

This code demonstrates a simple user authentication system, but it can be expanded to include more features like password reset, social media login, and more.