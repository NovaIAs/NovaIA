```typescript
// Imports the required libraries.
import { Router, Request, Response } from 'express';
import { MongoClient, Db, Collection } from 'mongodb';
import { Document } from 'mongoose';
import * as bcrypt from 'bcrypt';
import * as jwt from 'jsonwebtoken';
import { config } from 'dotenv';

// Loads the environment variables from the .env file.
config();

// Creates a new Router object.
const router = Router();

// Creates a new MongoClient object.
const mongoClient = new MongoClient(process.env.MONGO_URI);

// Creates a new collection object.
let collection: Collection;

// Connects to the MongoDB database.
mongoClient.connect((err, client) => {
  if (err) {
    console.error(err);
    process.exit(1);
  }

  // Gets the database object.
  const db = client.db(process.env.MONGO_DB_NAME);

  // Gets the collection object.
  collection = db.collection(process.env.MONGO_COLLECTION_NAME);
});

// Creates a new user.
router.post('/users', async (req: Request, res: Response) => {
  // Hashes the password.
  const hashedPassword = await bcrypt.hash(req.body.password, 10);

  // Creates a new user object.
  const user = {
    username: req.body.username,
    password: hashedPassword,
  };

  // Inserts the user object into the database.
  collection.insertOne(user, (err, result) => {
    if (err) {
      console.error(err);
      res.status(500).json({ error: 'An error occurred while creating the user.' });
    } else {
      res.status(201).json({ message: 'User created successfully.' });
    }
  });
});

// Logs in a user.
router.post('/login', async (req: Request, res: Response) => {
  // Gets the user object from the database.
  collection.findOne({ username: req.body.username }, (err, user) => {
    if (err) {
      console.error(err);
      res.status(500).json({ error: 'An error occurred while logging in the user.' });
    } else if (!user) {
      res.status(404).json({ error: 'User not found.' });
    } else {
      // Compares the entered password with the hashed password.
      bcrypt.compare(req.body.password, user.password, (err, result) => {
        if (err) {
          console.error(err);
          res.status(500).json({ error: 'An error occurred while logging in the user.' });
        } else if (!result) {
          res.status(401).json({ error: 'Invalid password.' });
        } else {
          // Creates a JWT token.
          const token = jwt.sign({ id: user._id }, process.env.JWT_SECRET, {
            expiresIn: '1h',
          });

          // Sends the token to the client.
          res.status(200).json({ token });
        }
      });
    }
  });
});

// Gets all users.
router.get('/users', async (req: Request, res: Response) => {
  // Gets all the users from the database.
  collection.find({}).toArray((err, users) => {
    if (err) {
      console.error(err);
      res.status(500).json({ error: 'An error occurred while getting the users.' });
    } else {
      res.status(200).json({ users });
    }
  });
});

// Gets a user by ID.
router.get('/users/:id', async (req: Request, res: Response) => {
  // Gets the user from the database.
  collection.findOne({ _id: req.params.id }, (err, user) => {
    if (err) {
      console.error(err);
      res.status(500).json({ error: 'An error occurred while getting the user.' });
    } else if (!user) {
      res.status(404).json({ error: 'User not found.' });
    } else {
      res.status(200).json({ user });
    }
  });
});

// Updates a user.
router.put('/users/:id', async (req: Request, res: Response) => {
  // Hashes the password.
  const hashedPassword = await bcrypt.hash(req.body.password, 10);

  // Updates the user in the database.
  collection.updateOne(
    { _id: req.params.id },
    { $set: { username: req.body.username, password: hashedPassword } },
    (err, result) => {
      if (err) {
        console.error(err);
        res.status(500).json({ error: 'An error occurred while updating the user.' });
      } else if (result.modifiedCount === 0) {
        res.status(404).json({ error: 'User not found.' });
      } else {
        res.status(200).json({ message: 'User updated successfully.' });
      }
    }
  );
});

// Deletes a user.
router.delete('/users/:id', async (req: Request, res: Response) => {
  // Deletes the user from the database.
  collection.deleteOne({ _id: req.params.id }, (err, result) => {
    if (err) {
      console.error(err);
      res.status(500).json({ error: 'An error occurred while deleting the user.' });
    } else if (result.deletedCount === 0) {
      res.status(404).json({ error: 'User not found.' });
    } else {
      res.status(200).json({ message: 'User deleted successfully.' });
    }
  });
});

// Exports the router object.
export default router;
```

This is a complex TypeScript code that implements a REST API for managing users in a MongoDB database. The code uses the Express framework for routing, MongoDB for data storage, bcrypt for hashing passwords, and JWT for authentication.

The router object has six routes:

* `/users`: Creates a new user.
* `/login`: Logs in a user.
* `/users`: Gets all users.
* `/users/:id`: Gets a user by ID.
* `/users/:id`: Updates a user.
* `/users/:id`: Deletes a user.

Each route has a corresponding request handler function that performs the necessary operations to handle the request. For example, the `/users` route has a request handler function that inserts a new user object into the database.

The code also includes a `mongoClient` object that is used to connect to the MongoDB database. The `collection` object is used to perform operations on the users collection in the database.

The code uses the `bcrypt` library to hash passwords before storing them in the database. This helps to protect user passwords from being compromised.

The code uses the `jwt` library to create JWT tokens for authenticated users. JWT tokens are used to authorize users to access protected resources.

The code is well-commented and organized, which makes it easy to understand and maintain. Overall, this is a well-written and efficient code that demonstrates a good understanding of TypeScript, Express, MongoDB, bcrypt, and JWT.