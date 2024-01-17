**Code:**

```javascript
// Import the necessary modules.
const fs = require('fs');
const path = require('path');
const express = require('express');

// Create an Express app.
const app = express();

// Serve static files from the public directory.
app.use(express.static('public'));

// Load the index.html file as the home page.
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.html'));
});

// Create a POST route to handle form submissions.
app.post('/submit', (req, res) => {
  // Parse the form data.
  const name = req.body.name;
  const email = req.body.email;
  const message = req.body.message;

  // Validate the form data.
  if (!name) {
    res.status(400).send('Name is required.');
    return;
  }

  if (!email) {
    res.status(400).send('Email is required.');
    return;
  }

  if (!message) {
    res.status(400).send('Message is required.');
    return;
  }

  // Write the form data to a file.
  fs.writeFile('data.txt', `${name}, ${email}, ${message}`, (err) => {
    if (err) {
      res.status(500).send('Error writing to file.');
      return;
    }

    // Send a success message.
    res.send('Thank you for your message.');
  });
});

// Start the server.
app.listen(3000, () => {
  console.log('Server is listening on port 3000.');
});
```

**Explanation:**

This code is a simple Express.js application that serves a static website and handles form submissions.

The first few lines of the code import the necessary modules:

* `fs`: This module provides functions for interacting with the file system.
* `path`: This module provides functions for working with file paths.
* `express`: This module provides a framework for building web applications.

The next few lines of the code create an Express app and configure it to serve static files from the `public` directory.

The `app.get()` method is used to define a route that handles GET requests to the root URL (`/`). When a GET request is made to the root URL, the `index.html` file is served.

The `app.post()` method is used to define a route that handles POST requests to the `/submit` URL. When a POST request is made to the `/submit` URL, the form data is parsed and validated. If the form data is valid, it is written to a file.

The `fs.writeFile()` method is used to write the form data to a file.

The `app.listen()` method is used to start the server. The server listens on port 3000.

This code is complex because it uses a number of different modules and techniques to handle form submissions. It also demonstrates how to use Express.js to build a simple web application.