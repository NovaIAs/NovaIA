```
UML Class Diagram for a Social Networking System:

 <<system>> Social Networking System

<<interface>> User

+ getName(): String
+ getEmail(): String
+ getPassword(): String
+ getFriends(): List<User>
+ addFriend(User): void
+ sendMessage(User, String): void

<<class>> StandardUser extends User

- isPremium: boolean

<<class>> PremiumUser extends User

- storageLimit: int
- canSendMessages: boolean

<<class>> AdminUser extends User

- canManageUsers: boolean
- canDeletePosts: boolean

<<class>> Post

+ getContent(): String
+ getAuthor(): User
+ getLikes(): List<User>
+ getComments(): List<Comment>
+ addLike(User): void
+ addComment(Comment): void

<<class>> Comment

+ getContent(): String
+ getAuthor(): User
+ getLikes(): List<User>
+ addLike(User): void

<<class>> FriendRequest

+ getSender(): User
+ getRecipient(): User
+ getStatus(): String

<<relationship>> Friendship

+ user1: User
+ user2: User
+ status: String

<<relationship>> Likes

+ user: User
+ post: Post

<<relationship>> Comments

+ user: User
+ post: Post

<<relationship>> FriendRequests

+ sender: User
+ recipient: User
```

Explanation:

- The class diagram models a social networking system that includes three types of users: standard users, premium users, and admin users.
- Standard users have basic features like sending messages and adding friends.
- Premium users have additional features like increased storage limit and the ability to send messages to users they're not friends with.
- Admin users have the ability to manage users and posts.
- The Post class represents posts that users can create and interact with. It has attributes like content, author, likes, and comments.
- The Comment class represents comments that users can add to posts. It has attributes like content, author, and likes.
- The FriendRequest class represents friend requests that users can send to each other.
- The Friendship class represents a relationship between two users who are friends.
- The Likes relationship represents users who have liked posts or comments.
- The Comments relationship represents users who have commented on posts.
- The FriendRequests relationship represents friend requests that have been sent or received.