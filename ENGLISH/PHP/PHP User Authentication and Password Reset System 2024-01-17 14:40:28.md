```php
<?php

// Define a function to generate a random string of a given length
function generateRandomString($length) {
  $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $charactersLength = strlen($characters);
  $randomString = '';
  for ($i = 0; $i < $length; $i++) {
    $randomString .= $characters[rand(0, $charactersLength - 1)];
  }
  return $randomString;
}

// Define a function to send an email
function sendEmail($recipient, $subject, $body) {
  $headers = [
    'From' => 'sender@example.com',
    'Reply-To' => 'sender@example.com',
    'Subject' => $subject,
    'Content-Type' => 'text/html'
  ];
  mail($recipient, $subject, $body, $headers);
}

// Define a class to represent a user
class User {

  private $id;
  private $username;
  private $email;
  private $password;

  public function __construct($username, $email, $password) {
    $this->username = $username;
    $this->email = $email;
    $this->password = $password;
  }

  public function getId() {
    return $this->id;
  }

  public function getUsername() {
    return $this->username;
  }

  public function getEmail() {
    return $this->email;
  }

  public function getPassword() {
    return $this->password;
  }

  public function setId($id) {
    $this->id = $id;
  }

  public function setUsername($username) {
    $this->username = $username;
  }

  public function setEmail($email) {
    $this->email = $email;
  }

  public function setPassword($password) {
    $this->password = $password;
  }

}

// Define a class to represent a database connection
class Database {

  private $connection;

  public function __construct() {
    $this->connection = new mysqli('localhost', 'root', '', 'database');
  }

  public function getConnection() {
    return $this->connection;
  }

  public function closeConnection() {
    $this->connection->close();
  }

}

// Define a function to connect to the database
function connectToDatabase() {
  $database = new Database();
  return $database->getConnection();
}

// Define a function to close the database connection
function closeDatabaseConnection($connection) {
  $connection->closeConnection();
}

// Define a function to register a new user
function registerUser($username, $email, $password) {
  $connection = connectToDatabase();
  $stmt = $connection->prepare('INSERT INTO users (username, email, password) VALUES (?, ?, ?)');
  $stmt->bind_param('sss', $username, $email, $password);
  $stmt->execute();
  $stmt->close();
  closeDatabaseConnection($connection);
}

// Define a function to login a user
function loginUser($username, $password) {
  $connection = connectToDatabase();
  $stmt = $connection->prepare('SELECT * FROM users WHERE username = ? AND password = ?');
  $stmt->bind_param('ss', $username, $password);
  $stmt->execute();
  $result = $stmt->get_result();
  $user = $result->fetch_object();
  $stmt->close();
  closeDatabaseConnection($connection);
  return $user;
}

// Define a function to send a password reset email
function sendPasswordResetEmail($email) {
  $connection = connectToDatabase();
  $stmt = $connection->prepare('SELECT * FROM users WHERE email = ?');
  $stmt->bind_param('s', $email);
  $stmt->execute();
  $result = $stmt->get_result();
  $user = $result->fetch_object();
  $stmt->close();
  closeDatabaseConnection($connection);
  if ($user) {
    $token = generateRandomString(32);
    $connection = connectToDatabase();
    $stmt = $connection->prepare('UPDATE users SET password_reset_token = ? WHERE email = ?');
    $stmt->bind_param('ss', $token, $email);
    $stmt->execute();
    $stmt->close();
    closeDatabaseConnection($connection);
    $subject = 'Password Reset';
    $body = 'Please click on the following link to reset your password: ' . 'http://example.com/reset-password?token=' . $token;
    sendEmail($email, $subject, $body);
  }
}

// Define a function to reset a password
function resetPassword($token, $newPassword) {
  $connection = connectToDatabase();
  $stmt = $connection->prepare('SELECT * FROM users WHERE password_reset_token = ?');
  $stmt->bind_param('s', $token);
  $stmt->execute();
  $result = $stmt->get_result();
  $user = $result->fetch_object();
  $stmt->close();
  closeDatabaseConnection($connection);
  if ($user) {
    $connection = connectToDatabase();
    $stmt = $connection->prepare('UPDATE users SET password = ?, password_reset_token = NULL WHERE email = ?');
    $stmt->bind_param('ss', $newPassword, $user->email);
    $stmt->execute();
    $stmt->close();
    closeDatabaseConnection($connection);
  }
}

// Main program
$action = isset($_GET['action']) ? $_GET['action'] : 'home';

switch ($action) {
  case 'register':
    $username = isset($_POST['username']) ? $_POST['username'] : '';
    $email = isset($_POST['email']) ? $_POST['email'] : '';
    $password = isset($_POST['password']) ? $_POST['password'] : '';
    registerUser($username, $email, $password);
    header('Location: login.php');
    break;
  case 'login':
    $username = isset($_POST['username']) ? $_POST['username'] : '';
    $password = isset($_POST['password']) ? $_POST['password'] : '';
    $user = loginUser($username, $password);
    if ($user) {
      session_start();
      $_SESSION['user_id'] = $user->getId