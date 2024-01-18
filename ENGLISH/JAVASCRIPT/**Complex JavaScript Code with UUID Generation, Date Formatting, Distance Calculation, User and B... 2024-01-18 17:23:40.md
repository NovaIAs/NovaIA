```javascript
// Define a function to generate a UUID
function uuidv4() {
  return ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}

// Define a function to format a date as a string
function formatDate(date) {
  const options = {
    year: 'numeric',
    month: 'long',
    day: 'numeric',
    hour: 'numeric',
    minute: 'numeric',
    second: 'numeric',
    timeZoneName: 'short',
  };
  return new Intl.DateTimeFormat('en-US', options).format(date);
}

// Define a function to calculate the distance between two points
function calculateDistance(lat1, lon1, lat2, lon2) {
  const R = 6371; // Earth's radius in kilometers
  const dLat = (lat2 - lat1) * (Math.PI / 180);
  const dLon = (lon2 - lon1) * (Math.PI / 180);
  const a =
    Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos((lat1 * Math.PI) / 180) *
      Math.cos((lat2 * Math.PI) / 180) *
      Math.sin(dLon / 2) *
      Math.sin(dLon / 2);
  const c = 2 * Math.asin(Math.sqrt(a));
  return R * c;
}

// Define a class to represent a user
class User {
  constructor(id, name, email, password, created_at) {
    this.id = id;
    this.name = name;
    this.email = email;
    this.password = password;
    this.created_at = created_at;
  }

  // Define a method to greet the user
  greet() {
    console.log(`Hello, ${this.name}!`);
  }
}

// Define a class to represent a book
class Book {
  constructor(id, title, author, pages, isbn, published_at) {
    this.id = id;
    this.title = title;
    this.author = author;
    this.pages = pages;
    this.isbn = isbn;
    this.published_at = published_at;
  }

  // Define a method to get the book's average rating
  getAverageRating() {
    const ratings = [3.5, 4.2, 4.8, 4.1, 4.7];
    return ratings.reduce((a, b) => a + b) / ratings.length;
  }
}

// Define a function to create a new user
function createUser(name, email, password) {
  const id = uuidv4();
  const created_at = new Date();
  const user = new User(id, name, email, password, created_at);
  return user;
}

// Define a function to create a new book
function createBook(title, author, pages, isbn, published_at) {
  const id = uuidv4();
  const book = new Book(id, title, author, pages, isbn, published_at);
  return book;
}

// Create a new user
const user = createUser('John Doe', 'johndoe@example.com', 'password123');

// Create a new book
const book = createBook(
  'The Lord of the Rings',
  'J.R.R. Tolkien',
  1207,
  '9780395082560',
  '1954-07-29'
);

// Greet the user
user.greet();

// Get the book's average rating
const averageRating = book.getAverageRating();

// Calculate the distance between two points
const distance = calculateDistance(
  37.78,
  -122.42,
  40.71,
  -74.00
);

// Format a date as a string
const dateString = formatDate(new Date());

// Log the results to the console
console.log(user);
console.log(book);
console.log(averageRating);
console.log(distance);
console.log(dateString);
```

This code is a complex and differentiated JavaScript code that includes a variety of functions and classes. It includes functions to generate a UUID, format a date, calculate the distance between two points, and create new users and books. It also includes a class to represent a user and a class to represent a book. The code is well-commented and easy to read, making it a good example of complex JavaScript code.