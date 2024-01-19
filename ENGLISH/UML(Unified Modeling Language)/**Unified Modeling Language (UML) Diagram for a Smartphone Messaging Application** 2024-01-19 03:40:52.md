```uml
+------------------------------+
|                              |
|    +----------------+      |
|    |  Smartphone App|      |
|    +----------------+      |
|                              |
+------------------------------+

     +--------------+            
     |  User       |            
     +--------------+            

+---------------------------------------------------------------------+
|                                                                     |
|      +---------------------+     +------------------------------+     |
|      | Conversation View   |_____| Sign In/Sign Up Screen   |     |
|      +---------------------+     +------------------------------+     |
|                                                                     |
+---------------------------------------------------------------------+

     +-------------+            
     |  Database   |            
     +-------------+            

    +-------+   +-------------+      +--------------+      
    |  User |-->| Conversation |----->| Message  |      
    +-------+   +-------------+      +--------------+      

     +-------------+            
     |  Server     |            
     +-------------+ 
```

* **Smartphone App:** This represents the mobile application that users interact with. It includes two main screens: the Conversation View and the Sign In/Sign Up Screen.
* **User:** This represents the individual using the application.
* **Database:** This is where all user data is stored, including conversations, messages, and user profiles.
* **Server:** This is the component that handles communication between the app and the database.

**Conversations:**

* A conversation is a collection of messages between two or more users.
* Conversations can be created and joined by users.
* Each conversation has a unique ID.
* Conversations can be viewed by users in the Conversation View.

**Messages:**

* A message is a text, image, or video sent by a user in a conversation.
* Messages have a timestamp indicating when they were sent.
* Messages are stored in the database.
* Messages can be viewed by users in the Conversation View.

**Users:**

* A user is an individual who uses the application.
* Users have a unique ID.
* Users have a username and password.
* Users can create and join conversations.
* Users can send and receive messages in conversations.
* User profiles are stored in the database.

**Sign In/Sign Up Screen:**

* This is the screen where users sign in to their existing accounts or create new accounts.
* Users enter their username and password to sign in.
* Users can create new accounts by providing a username, password, and email address.

**Conversation View:**

* This is the screen where users view conversations and send and receive messages.
* Users can view a list of all conversations they are participating in.
* Users can select a conversation to view the messages in it.
* Users can send new messages by typing them into a text field and pressing the Send button.

**Database:**

* The database is where all user data is stored.
* The database includes tables for users, conversations, messages, and user profiles.
* The database is managed by the server.

**Server:**

* The server is the component that handles communication between the app and the database.
* The server receives requests from the app and responds with data from the database.
* The server also sends notifications to users when they receive new messages.