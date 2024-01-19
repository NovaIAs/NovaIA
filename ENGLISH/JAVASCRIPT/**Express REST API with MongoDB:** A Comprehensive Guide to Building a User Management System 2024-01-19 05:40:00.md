```javascript
// Import the necessary libraries
const express = require('express');
const mongoose = require('mongoose');
const bodyParser = require('body-parser');
const helmet = require('helmet');
const rateLimit = require('express-rate-limit');
const morgan = require('morgan');

// Create an express app
const app = express();

// Use a helmet middleware to enhance the security of the application
app.use(helmet());

// Use a rate limiter middleware to limit the number of requests per IP address
app.use(rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100 // Limit each IP to 100 requests per windowMs
}));

// Use a body-parser middleware to parse JSON bodies
app.use(bodyParser.json());

// Connect to the MongoDB database
mongoose.connect('mongodb://localhost:27017/test', { useNewUrlParser: true, useUnifiedTopology: true });

// Define a schema for the users collection
const userSchema = new mongoose.Schema({
  name: String,
  email: String,
  password: String
});

// Create a model for the users collection
const User = mongoose.model('User', userSchema);

// Define a route to create a new user
app.post('/users', async (req, res) => {
  const user = new User(req.body);
  await user.save();
  res.json(user);
});

// Define a route to get all users
app.get('/users', async (req, res) => {
  const users = await User.find();
  res.json(users);
});

// Define a route to get a specific user by ID
app.get('/users/:id', async (req, res) => {
  const user = await User.findById(req.params.id);
  res.json(user);
});

// Define a route to update a specific user by ID
app.put('/users/:id', async (req, res) => {
  const user = await User.findByIdAndUpdate(req.params.id, req.body, { new: true });
  res.json(user);
});

// Define a route to delete a specific user by ID
app.delete('/users/:id', async (req, res) => {
  await User.findByIdAndDelete(req.params.id);
  res.json({ message: 'User deleted successfully' });
});

// Define a route to handle errors
app.use((err, req, res, next) => {
  res.status(500).json({ message: err.message });
});

// Use a morgan middleware to log requests
app.use(morgan('dev'));

// Start the server
app.listen(3000, () => {
  console.log('Server is listening on port 3000');
});
```

**Explanation:**

This code creates a simple REST API using Express.js and MongoDB. It defines a schema for the `users` collection in the database, and then defines routes to create, read, update, and delete users. The code also uses a number of middleware to enhance the security and performance of the application, such as Helmet, rate limiting, body parsing, and error handling. Finally, it uses Morgan to log requests to the console.

Here is a breakdown of the code:

* The first few lines import the necessary libraries.
* The `app` variable is created to represent the Express.js application.
* The `helmet()` middleware is used to enhance the security of the application.
* The `rateLimit()` middleware is used to limit the number of requests per IP address.
* The `bodyParser.json()` middleware is used to parse JSON bodies.
* The `mongoose.connect()` function is used to connect to the MongoDB database.
* The `userSchema` variable is created to define the schema for the `users` collection.
* The `User` model is created using the `mongoose.model()` function.
* A few routes are defined using the `app.post()`, `app.get()`, `app.put()`, and `app.delete()` methods. These routes handle the creation, retrieval, updating, and deletion of users, respectively.
* An error handling middleware is defined using the `app.use()` method. This middleware will handle any errors that occur in the application.
* The `morgan()` middleware is used to log requests to the console.
* The `app.listen()` function is used to start the server on port 3000.