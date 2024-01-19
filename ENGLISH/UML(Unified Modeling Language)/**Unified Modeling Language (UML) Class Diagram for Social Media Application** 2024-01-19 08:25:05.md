```
Domain Model:

Entity: User
- id: Integer (Primary Key)
- email: String (Unique)
- password: String (Hashed)
- name: String
- role: String (Admin, Moderator, User)

Entity: Post
- id: Integer (Primary Key)
- title: String
- content: Text
- author_id: Integer (Foreign Key to User)
- created_at: DateTime
- updated_at: DateTime

Entity: Comment
- id: Integer (Primary Key)
- content: Text
- author_id: Integer (Foreign Key to User)
- post_id: Integer (Foreign Key to Post)
- created_at: DateTime
- updated_at: DateTime

Entity: Like
- id: Integer (Primary Key)
- post_id: Integer (Foreign Key to Post)
- user_id: Integer (Foreign Key to User)
- created_at: DateTime

Usage Scenarios:

1. User Registration:
   - User provides email, password, and name.
   - System validates the email and password, and checks if the email is unique.
   - If validation passes, the system creates a new user record with the provided information and assigns a default role (e.g., User).

2. User Login:
   - User enters email and password.
   - System retrieves the user record with the given email address and compares the provided password with the hashed password in the database.
   - If the password matches, the system marks the user as logged in and generates a session token.

3. Create Post:
   - User provides a title, content, and selects a category.
   - System validates the title and content, and ensures that the category is valid.
   - If validation passes, the system creates a new post record and associates it with the current user.

4. View Post:
   - User selects a post from the list of posts.
   - System retrieves the post record and displays the title, content, author, and other relevant information.
   - If the user is logged in, the system also displays the like status and the list of comments.

5. Comment on Post:
   - User enters a comment text in the comment box.
   - System validates the comment text.
   - If validation passes, the system creates a new comment record and associates it with the current user and the selected post.

6. Like Post:
   - User clicks the like button on a post.
   - System checks if the user has already liked the post.
   - If not, the system creates a new like record and associates it with the current user and the selected post.

7. Delete Post:
   - User selects a post and clicks the delete button.
   - System checks if the user has the permission to delete the post (e.g., the post author or an administrator).
   - If the user has permission, the system deletes the post and all associated comments and likes.

8. Edit Post:
   - User selects a post and clicks the edit button.
   - System retrieves the post record and displays it in an editable form.
   - User makes changes to the title, content, or category.
   - System validates the changes and saves them to the database.

9. User Profile:
   - User clicks on their profile picture or username.
   - System retrieves the user's profile information, including name, email, role, and profile picture.
   - System displays the profile information and allows the user to update it.

10. Admin Dashboard:
    - Admin user logs in and navigates to the admin dashboard.
    - System displays a list of all users, posts, comments, and likes.
    - Admin user can perform various actions such as editing user roles, removing inappropriate content, and managing categories.
```

Explanation:

This UML class diagram represents a simplified social media application. It includes entities like User, Post, Comment, and Like, capturing their attributes and relationships. The diagram also describes various usage scenarios, covering key functionalities such as user registration, login, creating posts, commenting on posts, liking posts, deleting posts, editing posts, viewing user profiles, and managing the admin dashboard.

This diagram is valuable for understanding the application's functionality, data structures, and interactions between different components. It serves as a blueprint for developers to implement the application and ensures that all stakeholders have a clear understanding of the system's behavior.