```php
<?php
session_start();

// Connect to the database
$db = new PDO('mysql:host=localhost;dbname=my_database', 'username', 'password');

// Get the current user's ID
$user_id = $_SESSION['user_id'];

// Get the current user's data from the database
$stmt = $db->prepare('SELECT * FROM users WHERE id = ?');
$stmt->execute(array($user_id));
$user = $stmt->fetch();

// Get all of the user's friends from the database
$stmt = $db->prepare('SELECT * FROM friends WHERE user_id = ?');
$stmt->execute(array($user_id));
$friends = $stmt->fetchAll();

// Get all of the user's posts from the database
$stmt = $db->prepare('SELECT * FROM posts WHERE user_id = ?');
$stmt->execute(array($user_id));
$posts = $stmt->fetchAll();

// Get all of the user's comments from the database
$stmt = $db->prepare('SELECT * FROM comments WHERE user_id = ?');
$stmt->execute(array($user_id));
$comments = $stmt->fetchAll();

// Get all of the user's likes from the database
$stmt = $db->prepare('SELECT * FROM likes WHERE user_id = ?');
$stmt->execute(array($user_id));
$likes = $stmt->fetchAll();

// Get all of the user's shares from the database
$stmt = $db->prepare('SELECT * FROM shares WHERE user_id = ?');
$stmt->execute(array($user_id));
$shares = $stmt->fetchAll();

// Get all of the user's groups from the database
$stmt = $db->prepare('SELECT * FROM groups WHERE user_id = ?');
$stmt->execute(array($user_id));
$groups = $stmt->fetchAll();

// Get all of the user's events from the database
$stmt = $db->prepare('SELECT * FROM events WHERE user_id = ?');
$stmt->execute(array($user_id));
$events = $stmt->fetchAll();

// Get all of the user's photos from the database
$stmt = $db->prepare('SELECT * FROM photos WHERE user_id = ?');
$stmt->execute(array($user_id));
$photos = $stmt->fetchAll();

// Get all of the user's videos from the database
$stmt = $db->prepare('SELECT * FROM videos WHERE user_id = ?');
$stmt->execute(array($user_id));
$videos = $stmt->fetchAll();

// Get all of the user's audio files from the database
$stmt = $db->prepare('SELECT * FROM audio_files WHERE user_id = ?');
$stmt->execute(array($user_id));
$audio_files = $stmt->fetchAll();

// Get all of the user's documents from the database
$stmt = $db->prepare('SELECT * FROM documents WHERE user_id = ?');
$stmt->execute(array($user_id));
$documents = $stmt->fetchAll();

// Get all of the user's other files from the database
$stmt = $db->prepare('SELECT * FROM other_files WHERE user_id = ?');
$stmt->execute(array($user_id));
$other_files = $stmt->fetchAll();

// Output the user's data to the screen
echo '<h1>User Data</h1>';
echo '<ul>';
echo '<li>ID: ' . $user['id'] . '</li>';
echo '<li>Username: ' . $user['username'] . '</li>';
echo '<li>Email: ' . $user['email'] . '</li>';
echo '<li>Password: ' . $user['password'] . '</li>';
echo '</ul>';

// Output the user's friends to the screen
echo '<h1>Friends</h1>';
echo '<ul>';
foreach ($friends as $friend) {
  echo '<li>' . $friend['username'] . '</li>';
}
echo '</ul>';

// Output the user's posts to the screen
echo '<h1>Posts</h1>';
echo '<ul>';
foreach ($posts as $post) {
  echo '<li>' . $post['content'] . '</li>';
}
echo '</ul>';

// Output the user's comments to the screen
echo '<h1>Comments</h1>';
echo '<ul>';
foreach