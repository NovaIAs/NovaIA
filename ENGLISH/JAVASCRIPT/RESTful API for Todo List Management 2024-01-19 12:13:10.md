```javascript
// Import the necessary modules.
const express = require('express');
const bodyParser = require('body-parser');
const mongoose = require('mongoose');
const cors = require('cors');

// Create an express app.
const app = express();

// Use the body-parser middleware to parse JSON requests.
app.use(bodyParser.json());

// Use the cors middleware to allow cross-origin requests.
app.use(cors());

// Connect to the MongoDB database.
mongoose.connect('mongodb://localhost:27017/test', {
  useNewUrlParser: true,
  useUnifiedTopology: true
});

// Define the schema for the todo item model.
const todoSchema = new mongoose.Schema({
  title: {
    type: String,
    required: true
  },
  description: {
    type: String
  },
  completed: {
    type: Boolean,
    default: false
  }
});

// Create the todo item model.
const TodoItem = mongoose.model('TodoItem', todoSchema);

// Define the routes for the app.

// GET /todo-items
app.get('/todo-items', async (req, res) => {
  const todoItems = await TodoItem.find();
  res.json(todoItems);
});

// POST /todo-items
app.post('/todo-items', async (req, res) => {
  const todoItem = new TodoItem(req.body);
  await todoItem.save();
  res.json(todoItem);
});

// PUT /todo-items/:id
app.put('/todo-items/:id', async (req, res) => {
  const todoItem = await TodoItem.findById(req.params.id);
  todoItem.title = req.body.title;
  todoItem.description = req.body.description;
  todoItem.completed = req.body.completed;
  await todoItem.save();
  res.json(todoItem);
});

// DELETE /todo-items/:id
app.delete('/todo-items/:id', async (req, res) => {
  await TodoItem.findByIdAndDelete(req.params.id);
  res.json({ message: 'Todo item deleted successfully.' });
});

// Start the app.
app.listen(3000, () => {
  console.log('App is listening on port 3000.');
});
```

Explanation:

This code creates a simple RESTful API for a todo list application using JavaScript and the Express.js framework. It uses the Mongoose library for connecting to the MongoDB database and for defining the schema for the todo item model.

The code first imports the necessary modules and then creates an express app. It then uses the body-parser middleware to parse JSON requests and the cors middleware to allow cross-origin requests.

Next, it connects to the MongoDB database and defines the schema for the todo item model. It then creates the todo item model and defines the routes for the app.

The routes include GET, POST, PUT, and DELETE routes for the todo items. The GET route retrieves all the todo items from the database, the POST route creates a new todo item, the PUT route updates an existing todo item, and the DELETE route deletes a todo item.

Finally, the code starts the app on port 3000.