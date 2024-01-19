```javascript
// Import essential modules
const express = require('express');
const bodyParser = require('body-parser');
const mongoose = require('mongoose');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');

// Initialize the Express app
const app = express();

// Configure Express middleware
app.use(bodyParser.json());

// Connect to the MongoDB database
mongoose.connect('mongodb://localhost:27017/my-app', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the User schema for MongoDB
const userSchema = new mongoose.Schema({
  username: String,
  password: String
});

// Create the User model based on the schema
const User = mongoose.model('User', userSchema);

// Define the secret key for JSON Web Tokens
const secretKey = 'my-secret-key';

// Register a new user
app.post('/register', async (req, res) => {
  // Hash the password for security
  const hashedPassword = await bcrypt.hash(req.body.password, 10);

  // Create a new user with the hashed password
  const newUser = new User({
    username: req.body.username,
    password: hashedPassword
  });

  // Save the new user to the database
  await newUser.save();

  // Send a success response
  res.status(201).send({ message: 'User registered successfully' });
});

// Log in a user
app.post('/login', async (req, res) => {
  // Find the user with the given username
  const user = await User.findOne({ username: req.body.username });

  // If no user found, return an error
  if (!user) {
    return res.status(404).send({ message: 'User not found' });
  }

  // Compare the entered password with the hashed password in the database
  const isPasswordValid = await bcrypt.compare(req.body.password, user.password);

  // If the password is invalid, return an error
  if (!isPasswordValid) {
    return res.status(401).send({ message: 'Invalid password' });
  }

  // Create a JSON Web Token (JWT) for the user
  const token = jwt.sign({ username: user.username }, secretKey, { expiresIn: '1h' });

  // Send the JWT to the client
  res.status(200).send({ token });
});

// Verify a JWT and protect a route
app.get('/protected', async (req, res) => {
  // Get the JWT from the Authorization header
  const token = req.headers.authorization.split(' ')[1];

  // Verify the JWT using the secret key
  const decoded = jwt.verify(token, secretKey);

  // Find the user with the username from the decoded JWT
  const user = await User.findOne({ username: decoded.username });

  // If no user found, return an error
  if (!user) {
    return res.status(404).send({ message: 'User not found' });
  }

  // Send the user's data to the client
  res.status(200).send({ user });
});

// Start the Express server
app.listen(3000, () => {
  console.log('Server listening on port 3000');
});
```

Explanation:

This JavaScript code sets up a REST API using the Express framework and MongoDB for user registration, login, and protected route access with JSON Web Tokens (JWTs). Here's a breakdown of each section:

1. Module Imports:
   - Import necessary modules, including Express, BodyParser, Mongoose, bcrypt for password hashing, and jwt for JWT generation and verification.

2. Express App Initialization:
   - Create an Express app instance to handle HTTP requests and responses.

3. Middleware Configuration:
   - Use the BodyParser middleware to parse JSON requests.

4. Database Connection (MongoDB):
   - Establish a connection to the MongoDB database using Mongoose.

5. User Schema and Model:
   - Define the User schema for MongoDB, specifying fields like username and password.
   - Create the User model based on the schema, allowing for interactions with the database.

6. Secret Key for JWTs:
   - Define a secret key to be used for signing and verifying JWTs.

7. User Registration:
   - Handle POST requests to '/register' to create a new user.
   - Hash the password using bcrypt for security.
   - Save the new user to the database.
   - Send a success response.

8. User Login:
   - Handle POST requests to '/login' to log in a user.
   - Find the user in the database by username.
   - Compare the entered password with the hashed password.
   - Generate a JWT for the user if credentials are valid.
   - Send the JWT to the client.

9. Protected Route:
   - Handle GET requests to '/protected' to demonstrate JWT-protected routes.
   - Extract the JWT from the Authorization header.
   - Verify the JWT using the secret key.
   - Find the user associated with the JWT's username.
   - Send the user's data to the client.

10. Server Startup:
    - Start the Express server and listen for requests on port 3000.

This code demonstrates a more complex implementation with MongoDB, bcrypt for password hashing, JWTs for authentication, and a protected route, all while following good security practices.