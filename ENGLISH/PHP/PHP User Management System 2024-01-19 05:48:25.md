```php
<?php

// Define a function to generate a random string of a given length
function generateRandomString($length) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $randomString;
}

// Define a function to send an email to a given address with a given message
function sendEmail($to, $message) {
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();

    mail($to, 'Message from PHP Script', $message, $headers);
}

// Define a class to represent a user
class User {
    private $id;
    private $username;
    private $email;
    private $password;

    public function __construct($id, $username, $email, $password) {
        $this->id = $id;
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
        $this->connection = new mysqli('localhost', 'root', '', 'php_script_database');
        if ($this->connection->connect_error) {
            die('Could not connect to the database: ' . $this->connection->connect_error);
        }
    }

    public function getConnection() {
        return $this->connection;
    }

    public function closeConnection() {
        $this->connection->close();
    }
}

// Define a class to represent a user repository
class UserRepository {
    private $database;

    public function __construct(Database $database) {
        $this->database = $database;
    }

    public function findUserById($id) {
        $query = 'SELECT * FROM users WHERE id = ?';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('i', $id);
        $statement->execute();
        $result = $statement->get_result();
        if ($result->num_rows === 1) {
            $row = $result->fetch_assoc();
            return new User($row['id'], $row['username'], $row['email'], $row['password']);
        } else {
            return null;
        }
    }

    public function findUserByUsername($username) {
        $query = 'SELECT * FROM users WHERE username = ?';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('s', $username);
        $statement->execute();
        $result = $statement->get_result();
        if ($result->num_rows === 1) {
            $row = $result->fetch_assoc();
            return new User($row['id'], $row['username'], $row['email'], $row['password']);
        } else {
            return null;
        }
    }

    public function findUserByEmail($email) {
        $query = 'SELECT * FROM users WHERE email = ?';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('s', $email);
        $statement->execute();
        $result = $statement->get_result();
        if ($result->num_rows === 1) {
            $row = $result->fetch_assoc();
            return new User($row['id'], $row['username'], $row['email'], $row['password']);
        } else {
            return null;
        }
    }

    public function createUser(User $user) {
        $query = 'INSERT INTO users (username, email, password) VALUES (?, ?, ?)';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('sss', $user->getUsername(), $user->getEmail(), $user->getPassword());
        $statement->execute();
        $user->setId($this->database->getConnection()->insert_id);
        return $user;
    }

    public function updateUser(User $user) {
        $query = 'UPDATE users SET username = ?, email = ?, password = ? WHERE id = ?';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('sssi', $user->getUsername(), $user->getEmail(), $user->getPassword(), $user->getId());
        $statement->execute();
        return $user;
    }

    public function deleteUser(User $user) {
        $query = 'DELETE FROM users WHERE id = ?';
        $statement = $this->database->getConnection()->prepare($query);
        $statement->bind_param('i', $user->getId());
        $statement->execute();
        return true;
    }
}

// Define a class to represent a session
class Session {
    private $sessionId;

    public function __construct() {
        $this->sessionId = generateRandomString(32);
        session_start();
        $_SESSION['sessionId'] = $this->sessionId;
    }

    public function getSessionId() {
        return $this->sessionId;
    }

    public function destroySession() {
        session_destroy();
    }
}

// Define a class to represent a flash message
class FlashMessage {
    private $message;
    private $type;

    public function __construct($message, $type) {
        $