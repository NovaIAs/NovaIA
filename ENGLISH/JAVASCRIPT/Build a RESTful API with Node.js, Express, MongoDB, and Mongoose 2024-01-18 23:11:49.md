```javascript
// Import the necessary libraries
const express = require('express');
const bodyParser = require('body-parser');
const mongoose = require('mongoose');
const cors = require('cors');

// Create an express app
const app = express();

// Use the body-parser middleware to parse JSON requests
app.use(bodyParser.json());

// Use the cors middleware to enable cross-origin requests
app.use(cors());

// Connect to the MongoDB database
mongoose.connect('mongodb://localhost:27017/myDatabase', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the schema for the User model
const userSchema = new mongoose.Schema({
  name: String,
  email: String,
  password: String
});

// Create the User model
const User = mongoose.model('User', userSchema);

// Define the schema for the Product model
const productSchema = new mongoose.Schema({
  name: String,
  description: String,
  price: Number
});

// Create the Product model
const Product = mongoose.model('Product', productSchema);

// Define the API routes

// Create a new user
app.post('/users', async (req, res) => {
  const user = new User(req.body);
  await user.save();
  res.json(user);
});

// Get all users
app.get('/users', async (req, res) => {
  const users = await User.find();
  res.json(users);
});

// Get a user by ID
app.get('/users/:id', async (req, res) => {
  const user = await User.findById(req.params.id);
  res.json(user);
});

// Update a user by ID
app.put('/users/:id', async (req, res) => {
  const user = await User.findByIdAndUpdate(req.params.id, req.body, { new: true });
  res.json(user);
});

// Delete a user by ID
app.delete('/users/:id', async (req, res) => {
  await User.findByIdAndDelete(req.params.id);
  res.json({ message: 'User deleted successfully' });
});

// Create a new product
app.post('/products', async (req, res) => {
  const product = new Product(req.body);
  await product.save();
  res.json(product);
});

// Get all products
app.get('/products', async (req, res) => {
  const products = await Product.find();
  res.json(products);
});

// Get a product by ID
app.get('/products/:id', async (req, res) => {
  const product = await Product.findById(req.params.id);
  res.json(product);
});

// Update a product by ID
app.put('/products/:id', async (req, res) => {
  const product = await Product