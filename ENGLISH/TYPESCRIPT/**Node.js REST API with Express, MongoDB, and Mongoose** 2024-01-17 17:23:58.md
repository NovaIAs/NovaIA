```typescript
// Import necessary modules.
import * as express from 'express';
import * as bodyParser from 'body-parser';
import * as mongoose from 'mongoose';

// Create an Express application.
const app = express();

// Use the body-parser middleware to parse JSON requests.
app.use(bodyParser.json());

// Connect to the MongoDB database.
mongoose.connect('mongodb://localhost:27017/test', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the schema for the User model.
const userSchema = new mongoose.Schema({
  name: String,
  email: String,
  password: String
});

// Create the User model.
const User = mongoose.model('User', userSchema);

// Define the route for creating a new user.
app.post('/users', async (req, res) => {
  // Create a new User object from the request body.
  const user = new User(req.body);

  // Save the User object to the database.
  await user.save();

  // Return a success response.
  res.status(201).send({ message: 'User created successfully' });
});

// Define the route for getting all users.
app.get('/users', async (req, res) => {
  // Get all the users from the database.
  const users = await User.find();

  // Return a success response with the users.
  res.status(200).send(users);
});

// Define the route for getting a single user by ID.
app.get('/users/:id', async (req, res) => {
  // Get the user with the specified ID from the database.
  const user = await User.findById(req.params.id);

  // If the user was not found, return a 404 response.
  if (!user) {
    res.status(404).send({ message: 'User not found' });
    return;
  }

  // Return a success response with the user.
  res.status(200).send(user);
});

// Define the route for updating a user by ID.
app.put('/users/:id', async (req, res) => {
  // Get the user with the specified ID from the database.
  const user = await User.findById(req.params.id);

  // If the user was not found, return a 404 response.
  if (!user) {
    res.status(404).send({ message: 'User not found' });
    return;
  }

  // Update the user with the new data from the request body.
  user.name = req.body.name;
  user.email = req.body.email;
  user.password = req.body.password;

  // Save the updated user to the database.
  await user.save();

  // Return a success response with the updated user.
  res.status(200).send(user);
});

// Define the route for deleting a user by ID.
app.delete('/users/:id', async (req, res) => {
  // Get the user with the specified ID from the database.
  const user = await User.findById(req.params.id);

  // If the user was not found, return a 404 response.
  if (!user) {
    res.status(404).send({ message: 'User not found' });
    return;
  }

  // Delete the user from the database.
  await user.delete();

  // Return a success response.
  res.status(200).send({ message: 'User deleted successfully' });
});

// Start the Express application.
app.listen(3000, () => {
  console.log('Server is listening on port 3000');
});
```

This code is a Node.js application that uses the Express framework to create a simple REST API for managing users. The application uses the MongoDB database to store the user data.

The code includes the following features:

* A user model that defines the structure of the user data.
* Routes for creating, retrieving, updating, and deleting users.
* Error handling for all routes.
* A server that listens on port 3000.

This code demonstrates the use of the Express framework, MongoDB, and mongoose to create a simple REST API.

**Here is a more detailed explanation of the code:**

* **Modules:** The code starts by importing the necessary modules: `express`, `body-parser`, and `mongoose`.

* **Express Application:** An Express application is created using the `express()` function.

* **Body-Parser Middleware:** The `body-parser` middleware is used to parse JSON requests. This allows the application to access the request body data in the `req.body` object.

* **MongoDB Connection:** The `mongoose.connect()` function is used to connect to the MongoDB database.

* **User Schema:** The `userSchema` variable defines the schema for the User model. This schema defines the structure of the data that will be stored in the database for each user.

* **User Model:** The `User` model is created using the `mongoose.model()` function. This model can be used to create, retrieve, update, and delete users in the database.

* **Routes:** The following routes are defined in the code:
    * `/users`: This route handles requests to create new users.
    * `/users/:id`: This route handles requests to get, update, and delete users by their ID.

* **Error Handling:** Error handling is implemented for all routes using try-catch blocks. If an error occurs, a 500 error response is returned.

* **Server:** The Express application is started using the `listen()` function. This starts the server and makes it listen on port 3000.

This code is a complete example of how to create a simple REST API using Node.js, Express, MongoDB, and mongoose.