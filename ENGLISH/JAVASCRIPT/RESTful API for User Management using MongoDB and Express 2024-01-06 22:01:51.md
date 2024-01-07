```javascript
// Import the necessary modules.
const express = require('express');
const bodyParser = require('body-parser');
const mongoose = require('mongoose');

// Create an Express app.
const app = express();

// Use the body-parser middleware to parse JSON requests.
app.use(bodyParser.json());

// Connect to the MongoDB database.
mongoose.connect('mongodb://localhost:27017/my-database', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the schema for the 'users' collection.
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

// Create the 'users' model.
const User = mongoose.model('User', userSchema);

// Define the route for creating a new user.
app.post('/users', async (req, res) => {
  try {
    // Create a new user.
    const user = new User(req.body);

    // Save the user to the database.
    await user.save();

    // Send a response to the client.
    res.status(201).json({
      message: 'User created successfully',
      data: user
    });
  } catch (error) {
    // Handle the error.
    res.status(500).json({
      message: 'Error creating user',
      error: error
    });
  }
});

// Define the route for getting all users.
app.get('/users', async (req, res) => {
  try {
    // Get all users from the database.
    const users = await User.find();

    // Send a response to the client.
    res.status(200).json({
      message: 'Users retrieved successfully',
      data: users
    });
  } catch (error) {
    // Handle the error.
    res.status(500).json({
      message: 'Error retrieving users',
      error: error
    });
  }
});

// Define the route for getting a single user by ID.
app.get('/users/:id', async (req, res) => {
  try {
    // Get the user by ID from the database.
    const user = await User.findById(req.params.id);

    // Send a response to the client.
    res.status(200).json({
      message: 'User retrieved successfully',
      data: user
    });
  } catch (error) {
    // Handle the error.
    res.status(500).json({
      message: 'Error retrieving user',
      error: error
    });
  }
});

// Define the route for updating a user by ID.
app.put('/users/:id', async (req, res) => {
  try {
    // Update the user by ID in the database.
    const user = await User.findByIdAndUpdate(req.params.id, req.body, { new: true });

    // Send a response to the client.
    res.status(200).json({
      message: 'User updated successfully',
      data: user
    });
  } catch (error) {
    // Handle the error.
    res.status(500).json({
      message: 'Error updating user',
      error: error
    });
  }
});

// Define the route for deleting a user by ID.
app.delete('/users/:id', async (req, res) => {
  try {
    // Delete the user by ID from the database.
    await User.findByIdAndDelete(req.params.id);

    // Send a response to the client.
    res.status(200).json({
      message: 'User deleted successfully'
    });
  } catch (error) {
    // Handle the error.
    res.status(500).json({
      message: 'Error deleting user',
      error: error
    });
  }
});

// Start the Express app.
app.listen(3000, () => {
  console.log('App listening on port 3000');
});
```

This code is a RESTful API for managing users in a MongoDB database. It uses the Express framework for routing and the body-parser middleware for parsing JSON requests. The code includes routes for creating, retrieving, updating, and deleting users. The code also handles errors and sends appropriate responses to the client.

Here is a breakdown of the code:

* The `require()` statements at the beginning of the file import the necessary modules.
* The `app` variable is created to represent the Express app.
* The `app.use()` method is used to use the body-parser middleware to parse JSON requests.
* The `mongoose.connect()` method is used to connect to the MongoDB database.
* The `userSchema` variable is created to define the schema for the 'users' collection in the database.
* The `User` model is created using the `mongoose.model()` method.
* The `app.post()` method is used to define the route for creating a new user.
* The `app.get()` method is used to define the route for getting all users.
* The `app.get()` method is used to define the route for getting a single user by ID.
* The `app.put()` method is used to define the route for updating a user by ID.
* The `app.delete()` method is used to define the route for deleting a user by ID.
* The `app.listen()` method is used to start the Express app on port 3000.

This code is a complex and differentiated example of a JavaScript program. It uses a variety of modules and techniques to create a RESTful API for managing users in a MongoDB database.