```javascript
// Import necessary libraries
const express = require('express');
const bodyParser = require('body-parser');
const mongoose = require('mongoose');

// Initialize the Express app
const app = express();

// Use body-parser middleware to parse JSON requests
app.use(bodyParser.json());

// Connect to the MongoDB database
mongoose.connect('mongodb://localhost:27017/myDatabase', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the schema for the 'users' collection
const userSchema = new mongoose.Schema({
  name: String,
  email: String,
  password: String
});

// Create the 'users' model
const User = mongoose.model('User', userSchema);

// Define the routes for the API

// Create a new user
app.post('/users', async (req, res) => {
  const newUser = new User(req.body);
  await newUser.save();
  res.status(201).send(newUser);
});

// Get all users
app.get('/users', async (req, res) => {
  const users = await User.find();
  res.send(users);
});

// Get a user by ID
app.get('/users/:id', async (req, res) => {
  const user = await User.findById(req.params.id);
  if (!user) {
    res.status(404).send('User not found');
  } else {
    res.send(user);
  }
});

// Update a user by ID
app.put('/users/:id', async (req, res) => {
  const user = await User.findByIdAndUpdate(req.params.id, req.body, { new: true });
  if (!user) {
    res.status(404).send('User not found');
  } else {
    res.send(user);
  }
});

// Delete a user by ID
app.delete('/users/:id', async (req, res) => {
  const user = await User.findByIdAndDelete(req.params.id);
  if (!user) {
    res.status(404).send('User not found');
  } else {
    res.send(user);
  }
});

// Start the server
app.listen(3000, () => {
  console.log('Server is listening on port 3000');
});
```

Explanation:

This code is a Node.js script that uses the Express framework to create a RESTful API for managing users in a MongoDB database. The code includes:

- Importing the necessary libraries: Express, body-parser, and mongoose.
- Initializing the Express app.
- Using body-parser middleware to parse JSON requests.
- Connecting to the MongoDB database.
- Defining the schema for the 'users' collection in MongoDB.
- Creating the 'users' model using the Mongoose schema.
- Defining the routes for the API, including endpoints for creating, getting, updating, and deleting users.
- Starting the server on port 3000.

The code is well-organized and uses modern JavaScript syntax, including async/await for asynchronous operations. The routes are defined using the Express router, and the database operations are performed using the Mongoose ODM (Object-Data Mapper).

This code is a good example of how to use Express and Mongoose to create a RESTful API in Node.js. It can be used as a starting point for building more complex applications.