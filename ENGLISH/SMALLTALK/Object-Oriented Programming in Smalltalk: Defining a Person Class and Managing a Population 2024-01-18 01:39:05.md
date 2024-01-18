```smalltalk
Object subclass: Person [
  instanceVariableNames: 'name' 'age' 'gender'
  classVariableNames: 'population'
]

Person class methods for: accessing [
  population := OrderedCollection new.
  
  add: aPerson
	"Add a person to the population."
	population add: aPerson.
  
  size
	"Return the number of people in the population."
	^ population size
]

Person class methods for: querying [
  averageAge
	"Return the average age of all people in the population."
	| totalAge |
	totalAge := 0.
	population do: [ :each | totalAge := totalAge + each age ].
	^ totalAge / self size
]

Person class methods for: reporting [
  report
	"Print a report on the population."
	Transcript show: 'Population report:' cr.
	Transcript show: '---------------------' cr.
	Transcript show: 'Number of people: ' , self size , cr.
	Transcript show: 'Average age: ' , self averageAge , cr.
	Transcript show: '' cr.
]

Person methods for: accessing [
  name
	"Return the person's name."
	^ name
  
  age
	"Return the person's age."
	^ age
  
  gender
	"Return the person's gender."
	^ gender
]

Person methods for: modifying [
  name: aString
	"Set the person's name to aString."
	name := aString
  
  age: anInteger
	"Set the person's age to anInteger."
	age := anInteger
  
  gender: aString
	"Set the person's gender to aString."
	gender := aString
]

Person methods for: printing [
  printOn: aStream
	"Print the person's name, age, and gender on aStream."
	aStream show: name , ' (' , age , ', ' , gender , ')'
]

"Create some people and add them to the population."
Person new name: 'John' age: 25 gender: 'male'.
Person new name: 'Mary' age: 20 gender: 'female'.
Person new name: 'Tom' age: 30 gender: 'male'.

"Print a report on the population."
Person report
```

This code defines a class called `Person` in Smalltalk. A class in Smalltalk is a blueprint for creating objects. The `Person` class has three instance variables: `name`, `age`, and `gender`. Instance variables are like fields in Java or C++. They store the data for each individual object.

The `Person` class also has three class variables: `population`, `size`, and `averageAge`. Class variables are shared among all objects of a class. In this case, the `population` class variable stores a collection of all the `Person` objects that have been created. The `size` class variable stores the number of people in the population. The `averageAge` class variable stores the average age of all the people in the population.

The `Person` class has a number of methods. Methods are like functions in Java or C++. They perform actions on objects. The `Person` class has methods for accessing the instance variables, modifying the instance variables, printing the instance variables, and adding people to the population.

The `Person` class also has a number of class methods. Class methods are like static methods in Java or C++. They can be called without creating an instance of the class. The `Person` class has class methods for accessing the class variables, querying the population, and reporting on the population.

The last part of the code creates three `Person` objects and adds them to the population. Then, it prints a report on the population.

This code demonstrates a number of Smalltalk features, including classes, objects, instance variables, class variables, methods, and class methods. It also shows how to use Smalltalk to create and manage a collection of objects.