```groovy
// Create a Map to store the users
def users = [:]

// Create a List to store the posts
def posts = []

// Create a List to store the comments
def comments = []

// Create a List to store the tags
def tags = []

// Create a List to store the user-post relationships
def userPostRelationships = []

// Create a List to store the post-comment relationships
def postCommentRelationships = []

// Create a List to store the post-tag relationships
def postTagRelationships = []

// Create a List to store the tag-comment relationships
def tagCommentRelationships = []

// Add users to the Map
users['user1'] = [name: 'John Doe', email: 'john.doe@example.com']
users['user2'] = [name: 'Jane Doe', email: 'jane.doe@example.com']
users['user3'] = [name: 'John Smith', email: 'john.smith@example.com']

// Add posts to the List
posts << [title: 'Post 1', content: 'This is the first post.', author: 'user1']
posts << [title: 'Post 2', content: 'This is the second post.', author: 'user2']
posts << [title: 'Post 3', content: 'This is the third post.', author: 'user3']

// Add comments to the List
comments << [content: 'This is the first comment.', author: 'user1', post: 'post1']
comments << [content: 'This is the second comment.', author: 'user2', post: 'post2']
comments << [content: 'This is the third comment.', author: 'user3', post: 'post3']

// Add tags to the List
tags << [name: 'tag1']
tags << [name: 'tag2']
tags << [name: 'tag3']

// Add user-post relationships to the List
userPostRelationships << [user: 'user1', post: 'post1']
userPostRelationships << [user: 'user2', post: 'post2']
userPostRelationships << [user: 'user3', post: 'post3']

// Add post-comment relationships to the List
postCommentRelationships << [post: 'post1', comment: 'comment1']
postCommentRelationships << [post: 'post2', comment: 'comment2']
postCommentRelationships << [post: 'post3', comment: 'comment3']

// Add post-tag relationships to the List
postTagRelationships << [post: 'post1', tag: 'tag1']
postTagRelationships << [post: 'post2', tag: 'tag2']
postTagRelationships << [post: 'post3', tag: 'tag3']

// Add tag-comment relationships to the List
tagCommentRelationships << [tag: 'tag1', comment: 'comment1']
tagCommentRelationships << [tag: 'tag2', comment: 'comment2']
tagCommentRelationships << [tag: 'tag3', comment: 'comment3']

// Print the users
println 'Users:'
users.each { println it }

// Print the posts
println 'Posts:'
posts.each { println it }

// Print the comments
println 'Comments:'
comments.each { println it }

// Print the tags
println 'Tags:'
tags.each { println it }

// Print the user-post relationships
println 'User-Post Relationships:'
userPostRelationships.each { println it }

// Print the post-comment relationships
println 'Post-Comment Relationships:'
postCommentRelationships.each { println it }

// Print the post-tag relationships
println 'Post-Tag Relationships:'
postTagRelationships.each { println it }

// Print the tag-comment relationships
println 'Tag-Comment Relationships:'
tagCommentRelationships.each { println it }
```

This code creates a complex data structure in Groovy. It defines a Map to store users, a List to store posts, a List to store comments, a List to store tags, a List to store user-post relationships, a List to store post-comment relationships, a List to store post-tag relationships, and a List to store tag-comment relationships. It then populates these data structures with data and prints them out.

This code is complex because it uses a variety of Groovy features, including Maps, Lists, and closures. It also uses a variety of Groovy syntax, including the `<<` operator for adding items to a List, the `each` method for iterating over a collection, and the `println` method for printing to the console.

This code is useful for demonstrating the power and flexibility of Groovy. It can be used to create complex data structures and relationships between objects in a concise and easy-to-read manner.