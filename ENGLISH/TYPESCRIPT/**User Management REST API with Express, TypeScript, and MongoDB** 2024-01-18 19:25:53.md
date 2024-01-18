```typescript
// Import necessary libraries
import * as express from 'express';
import * as bodyParser from 'body-parser';
import * as mongoose from 'mongoose';

// Initialize an Express application
const app = express();

// Parse incoming JSON requests
app.use(bodyParser.json());

// Connect to MongoDB
mongoose.connect('mongodb://localhost:27017/my_database', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define a schema for our data model
const userSchema = new mongoose.Schema({
  name: {
    type: String,
    required: true
  },
  email: {
    type: String,
    required: true,
    unique: true
  },
  password: {
    type: String,
    required: true
  }
});

// Create a model using our schema
const User = mongoose.model('User', userSchema);

// Define a route to handle POST requests to create a new user
app.post('/users', async (req, res) => {
  try {
    // Create a new user document
    const newUser = new User(req.body);

    // Save the new user document to the database
    await newUser.save();

    // Send a success response
    res.status(201).send({
      message: 'User created successfully'
    });
  } catch (err) {
    // Handle any errors that occurred during the save operation
    res.status(500).send({
      error: 'An error occurred while creating the user'
    });
  }
});

// Define a route to handle GET requests to fetch all users
app.get('/users', async (req, res) => {
  try {
    // Find all users in the database
    const users = await User.find();

    // Send the users back as a response
    res.status(200).send(users);
  } catch (err) {
    // Handle any errors that occurred during the find operation
    res.status(500).send({
      error: 'An error occurred while fetching the users'
    });
  }
});

// Define a route to handle GET requests to fetch a single user by ID
app.get('/users/:id', async (req, res) => {
  try {
    // Find the user with the specified ID
    const user = await User.findById(req.params.id);

    // If the user was found, send it back as a response
    if (user) {
      res.status(200).send(user);
    } else {
      // If the user was not found, send a not found response
      res.status(404).send({
        error: 'User not found'
      });
    }
  } catch (err) {
    // Handle any errors that occurred during the find operation
    res.status(500).send({
      error: 'An error occurred while fetching the user'
    });
  }
});

// Define a route to handle PUT requests to update a user's information
app.put('/users/:id', async (req, res) => {
  try {
    // Find the user with the specified ID
    const user = await User.findById(req.params.id);

    // If the user was found, update its information and save it to the database
    if (user) {
      user.name = req.body.name;
      user.email = req.body.email;
      user.password = req.body.password;
      await user.save();

      // Send a success response
      res.status(200).send({
        message: 'User updated successfully'
      });
    } else {
      // If the user was not found, send a not found response
      res.status(404).send({
        error: 'User not found'
      });
    }
  } catch (err) {
    // Handle any errors that occurred during the update operation
    res.status(500).send({
      error: 'An error occurred while updating the user'
    });
  }
});

// Define a route to handle DELETE requests to delete a user
app.delete('/users/:id', async (req, res) => {
  try {
    // Find the user with the specified ID
    const user = await User.findById(req.params.id);

    // If the user was found, delete it from the database
    if (user) {
      await user.delete();

      // Send a success response
      res.status(200).send({
        message: 'User deleted successfully'
      });
    } else {
      // If the user was not found, send a not found response
      res.status(404).send({
        error: 'User not found'
      });
    }
  } catch (err) {
    // Handle any errors that occurred during the delete operation
    res.status(500).send({
      error: 'An error occurred while deleting the user'
    });
  }
});

// Start the Express application
app.listen(3000, () => {
  console.log('Server is listening on port 3000');
});
```

This is a more complex TypeScript code that demonstrates the use of Express, BodyParser, and MongoDB for building a simple REST API. It includes routes for creating, fetching, updating, and deleting users.

Here's a brief explanation of the code:

1. **Importing Libraries**: We start by importing all the necessary libraries including Express, BodyParser, and Mongoose.

2. **Initializing Express Application**: We create an Express application instance and configure it to parse incoming JSON requests.

3. **Connecting to MongoDB**: We establish a connection to the MongoDB database using the Mongoose library.

4. **Defining a Schema**: We define a schema for our User model using the Mongoose schema API. The schema defines the fields and their data types for the User model.

5. **Creating a Model**: We use the schema to create a User model using the Mongoose model() function. This model represents a collection in the MongoDB database.

6. **Defining Routes**: We define several routes using the Express router to handle different HTTP requests for our API:
   - `/users`: This route handles POST requests to create new users.
   - `/users`: This route handles GET requests to fetch all users from the database.
   - `/users/:id`: This route handles GET requests to fetch a single user by their ID.
   - `/users/:id`: This route handles PUT requests to update a user's information.
   - `/users/:id`: This route handles DELETE requests to delete a user from the database.

7. **Handling Requests**: Inside each route handler, we perform the appropriate operations on the User model, such as creating new users, finding existing users, updating user information, or deleting users.

8. **Starting the Express Application**: Finally, we start the Express application on port 3000 using the listen() method.

This code demonstrates a more comprehensive example of building a REST API using TypeScript, Express, and MongoDB. It includes more complex functionalities such as data validation, error handling, and more robust routing.