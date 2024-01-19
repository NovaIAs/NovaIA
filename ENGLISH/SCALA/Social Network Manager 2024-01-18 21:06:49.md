```scala
// Define a case class to represent a person with a name and age
case class Person(name: String, age: Int)

// Define a trait to represent a social network
trait SocialNetwork {
  // Method to add a person to the network
  def addPerson(person: Person): Unit

  // Method to remove a person from the network
  def removePerson(person: Person): Unit

  // Method to get all the people in the network
  def getAllPeople(): List[Person]

  // Method to get all the friends of a person
  def getFriends(person: Person): List[Person]

  // Method to add a friend relationship between two people
  def addFriend(person1: Person, person2: Person): Unit

  // Method to remove a friend relationship between two people
  def removeFriend(person1: Person, person2: Person): Unit

  // Method to check if two people are friends
  def areFriends(person1: Person, person2: Person): Boolean
}

// Define a class to represent a Facebook social network
class Facebook extends SocialNetwork {
  // Map to store the people in the network
  private val people = scala.collection.mutable.Map[String, Person]()

  // Map to store the friend relationships between people
  private val friends = scala.collection.mutable.Map[String, List[String]]()

  // Method to add a person to the network
  override def addPerson(person: Person): Unit = {
    people(person.name) = person
    friends(person.name) = List()
  }

  // Method to remove a person from the network
  override def removePerson(person: Person): Unit = {
    people.remove(person.name)
    friends.remove(person.name)
    for (friend <- friends.values) {
      friend.remove(person.name)
    }
  }

  // Method to get all the people in the network
  override def getAllPeople(): List[Person] = {
    people.values.toList
  }

  // Method to get all the friends of a person
  override def getFriends(person: Person): List[Person] = {
    friends(person.name).map(people(_))
  }

  // Method to add a friend relationship between two people
  override def addFriend(person1: Person, person2: Person): Unit = {
    friends(person1.name) = friends(person1.name) :+ person2.name
    friends(person2.name) = friends(person2.name) :+ person1.name
  }

  // Method to remove a friend relationship between two people
  override def removeFriend(person1: Person, person2: Person): Unit = {
    friends(person1.name) = friends(person1.name).filterNot(_ == person2.name)
    friends(person2.name) = friends(person2.name).filterNot(_ == person1.name)
  }

  // Method to check if two people are friends
  override def areFriends(person1: Person, person2: Person): Boolean = {
    friends(person1.name).contains(person2.name)
  }
}

// Define a class to represent a Twitter social network
class Twitter extends SocialNetwork {
  // Map to store the people in the network
  private val people = scala.collection.mutable.Map[String, Person]()

  // Map to store the followers of each person
  private val followers = scala.collection.mutable.Map[String, List[String]]()

  // Map to store the people that each person follows
  private val following = scala.collection.mutable.Map[String, List[String]]()

  // Method to add a person to the network
  override def addPerson(person: Person): Unit = {
    people(person.name) = person
    followers(person.name) = List()
    following(person.name) = List()
  }

  // Method to remove a person from the network
  override def removePerson(person: Person): Unit = {
    people.remove(person.name)
    followers.remove(person.name)
    following.remove(person.name)
    for (follower <- followers.values) {
      follower.remove(person.name)
    }
    for (following <- following.values) {
      following.remove(person.name)
    }
  }

  // Method to get all the people in the network
  override def getAllPeople(): List[Person] = {
    people.values.toList
  }

  // Method to get all the followers of a person
  override def getFriends(person: Person): List[Person] = {
    followers(person.name).map(people(_))
  }

  // Method to add a follower relationship between two people
  override def addFriend(person1: Person, person2: Person): Unit = {
    followers(person2.name) = followers(person2.name) :+ person1.name
    following(person1.name) = following(person1.name) :+ person2.name
  }

  // Method to remove a follower relationship between two people
  override def removeFriend(person1: Person, person2: Person): Unit = {
    followers(person2.name) = followers(person2.name).filterNot(_ == person1.name)
    following(person1.name) = following(person1.name).filterNot(_ == person2.name)
  }

  // Method to check if two people are friends
  override def areFriends(person1: Person, person2: Person): Boolean = {
    followers(person2.name).contains(person1.name)
  }
}

// Define a class to represent a LinkedIn social network
class LinkedIn extends SocialNetwork {
  // Map to store the people in the network
  private val people = scala.collection.mutable.Map[String, Person]()

  // Map to store the connections of each person
  private val connections = scala.collection.mutable.Map[String, List