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

// Define a function to send an email
function sendEmail($to, $subject, $message) {
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();
    mail($to, $subject, $message, $headers);
}

// Define a function to create a new user account
function createUserAccount($username, $password, $email) {
    $hashedPassword = password_hash($password, PASSWORD_DEFAULT);
    $activationCode = generateRandomString(16);
    $sql = 'INSERT INTO users (username, password, email, activation_code) VALUES (?, ?, ?, ?)';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('ssss', $username, $hashedPassword, $email, $activationCode);
    $stmt->execute();
    $stmt->close();

    // Send an email to the user with the activation code
    $subject = 'Activate Your Account';
    $message = 'Please click on the following link to activate your account: ' .
        'http://example.com/activate.php?code=' . $activationCode;
    sendEmail($email, $subject, $message);
}

// Define a function to activate a user account
function activateUserAccount($activationCode) {
    $sql = 'SELECT * FROM users WHERE activation_code = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('s', $activationCode);
    $stmt->execute();
    $result = $stmt->get_result();
    $user = $result->fetch_assoc();
    $stmt->close();

    if ($user) {
        $sql = 'UPDATE users SET activated = 1 WHERE id = ?';
        $stmt = $conn->prepare($sql);
        $stmt->bind_param('i', $user['id']);
        $stmt->execute();
        $stmt->close();

        // Send an email to the user confirming their account activation
        $subject = 'Account Activated';
        $message = 'Your account has been activated. You can now log in to the website.';
        sendEmail($user['email'], $subject, $message);
    }
}

// Define a function to log in a user
function loginUser($username, $password) {
    $sql = 'SELECT * FROM users WHERE username = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('s', $username);
    $stmt->execute();
    $result = $stmt->get_result();
    $user = $result->fetch_assoc();
    $stmt->close();

    if ($user && password_verify($password, $user['password'])) {
        $_SESSION['user_id'] = $user['id'];
        $_SESSION['username'] = $user['username'];
        return true;
    } else {
        return false;
    }
}

// Define a function to get the current user's ID
function getCurrentUserId() {
    if (isset($_SESSION['user_id'])) {
        return $_SESSION['user_id'];
    } else {
        return null;
    }
}

// Define a function to get the current user's username
function getCurrentUsername() {
    if (isset($_SESSION['username'])) {
        return $_SESSION['username'];
    } else {
        return null;
    }
}

// Define a function to check if the current user is logged in
function isLoggedIn() {
    return isset($_SESSION['user_id']);
}

// Define a function to log out the current user
function logoutUser() {
    session_destroy();
}

// Define a function to create a new post
function createPost($title, $content) {
    $sql = 'INSERT INTO posts (title, content, author_id) VALUES (?, ?, ?)';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('ssi', $title, $content, getCurrentUserId());
    $stmt->execute();
    $stmt->close();
}

// Define a function to get all posts
function getAllPosts() {
    $sql = 'SELECT * FROM posts ORDER BY created_at DESC';
    $stmt = $conn->prepare($sql);
    $stmt->execute();
    $result = $stmt->get_result();
    $posts = $result->fetch_all(MYSQLI_ASSOC);
    $stmt->close();

    return $posts;
}

// Define a function to get a single post by its ID
function getPostById($id) {
    $sql = 'SELECT * FROM posts WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('i', $id);
    $stmt->execute();
    $result = $stmt->get_result();
    $post = $result->fetch_assoc();
    $stmt->close();

    return $post;
}

// Define a function to update a post
function updatePost($id, $title, $content) {
    $sql = 'UPDATE posts SET title = ?, content = ? WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('ssi', $title, $content, $id);
    $stmt->execute();
    $stmt->close();
}

// Define a function to delete a post
function deletePost($id) {
    $sql = 'DELETE FROM posts WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('i', $id);
    $stmt->execute();
    $stmt->close();
}

// Define a function to create a new comment
function createComment($post_id, $content) {
    $sql = 'INSERT INTO comments (post_id, content, author_id) VALUES (?, ?, ?)';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('iis', $post_id, $content, getCurrentUserId());
    $stmt->execute();
    $stmt->close();
}

// Define a function to get all comments for a given post
function getCommentsForPost($post_id) {
    $sql = 'SELECT * FROM comments WHERE post_id = ? ORDER BY created_at DESC';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('i', $post_id);
    $stmt->execute();
    $result = $stmt->get_result();
    $comments = $result->fetch_all(MYSQLI_ASSOC);
    $stmt->close();

    return $comments;
}

// Define a function to get a single comment by its ID
function getCommentById($id) {
    $sql = 'SELECT * FROM comments WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('i', $id);
    $stmt->execute();
    $result = $stmt->get_result();
    $comment = $result->fetch_assoc();
    $stmt->close();

    return $comment;
}

// Define a function to update a comment
function updateComment($id, $content) {
    $sql = 'UPDATE comments SET content = ? WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('si', $content, $id);
    $stmt->execute();
    $stmt->close();
}

// Define a function to delete a comment
function deleteComment($id) {
    $sql = 'DELETE FROM comments WHERE id = ?';
    $stmt = $conn->prepare($sql);
    $stmt->bind_param('i', $id);
    $stmt->execute();
    $stmt->close();
}

// Define a function to get all users
function getAllUsers() {
    $sql = 'SELECT * FROM users ORDER BY created_at DESC';
    $stmt = $conn->prepare($sql);
    $stmt->execute();
    $result = $stmt->get_result();
    $users = $result->fetch_all(MYSQLI_ASSOC);
    $stmt->close();

    return $users;
}

// Define a function to get a single user by their ID
function getUserById($id) {
    $sql = 'SELECT *