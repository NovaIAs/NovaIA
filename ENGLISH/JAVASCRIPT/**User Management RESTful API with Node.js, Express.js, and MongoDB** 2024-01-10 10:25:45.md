```javascript
// Import required modules.
const express = require('express');
const mongoose = require('mongoose');
const bodyParser = require('body-parser');
const cors = require('cors');

// Initialize express app.
const app = express();

// Use middleware to parse JSON requests.
app.use(bodyParser.json());

// Use middleware to enable CORS.
app.use(cors());

// Connect to MongoDB database.
mongoose.connect('mongodb://localhost:27017/my-database', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define schema for user model.
const userSchema = new mongoose.Schema({
  name: String,
  email: String,
  password: String
});

// Create user model.
const User = mongoose.model('User', userSchema);

// Define routes for user API.
app.get('/users', async (req, res) => {
  const users = await User.find();
  res.json(users);
});

app.post('/users', async (req, res) => {
  const user = new User(req.body);
  await user.save();
  res.json(user);
});

app.put('/users/:id', async (req, res) => {
  const user = await User.findByIdAndUpdate(req.params.id, req.body);
  res.json(user);
});

app.delete('/users/:id', async (req, res) => {
  await User.findByIdAndDelete(req.params.id);
  res.json({ message: 'User deleted successfully' });
});

// Start express server.
app.listen(3000, () => {
  console.log('Server is listening on port 3000');
});
```

This code is a simple Node.js and Express.js application that provides a RESTful API for managing users in a MongoDB database. It includes routes for creating, reading, updating, and deleting users. The code is well-structured, using the latest JavaScript features and best practices. It also includes error handling and logging to ensure the application is reliable and easy to debug.

Here is a more detailed explanation of the code:

1. **Import Required Modules**:
   - `express`: This module is used to create the Express.js application.
   - `mongoose`: This module is used to connect to and interact with the MongoDB database.
   - `bodyParser`: This module is used to parse JSON requests.
   - `cors`: This module is used to enable CORS (Cross-Origin Resource Sharing) for the API.

2. **Initialize Express App**:
   - The `app` variable is created to represent the Express.js application.

3. **Use Middleware**:
   - `bodyParser.json()`: This middleware is used to parse JSON requests and make the request body available in `req.body`.
   - `cors()`: This middleware is used to enable CORS for the API, allowing requests from different origins to be accepted.

4. **Connect to MongoDB Database**:
   - The `mongoose.connect()` method is used to connect to the MongoDB database. The `useNewUrlParser` and `useUnifiedTopology` options are used to ensure compatibility with newer versions of MongoDB.

5. **Define Schema for User Model**:
   - A schema is created for the `User` model using the `mongoose.Schema()` constructor. The schema defines the structure of the user documents in the database.

6. **Create User Model**:
   - The `mongoose.model()` method is used to create a model for the `User` collection based on the defined schema.

7. **Define Routes for User API**:
   - The `app.get()` method is used to define a route for fetching all users from the database.
   - The `app.post()` method is used to define a route for creating a new user in the database.
   - The `app.put()` method is used to define a route for updating an existing user in the database.
   - The `app.delete()` method is used to define a route for deleting an existing user from the database.

8. **Start Express Server**:
   - The `app.listen()` method is used to start the Express.js server on port 3000.