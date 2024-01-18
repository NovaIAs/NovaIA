```smalltalk
1. Creating a Class "Person":
```

```smalltalk
Object subclass: Person instanceVariableNames: 'name age' classVariableNames: ''
```

* The `Object` class is the superclass of all classes in Smalltalk.
* `Person` is a subclass of `Object`.
* `instanceVariableNames: 'name age'` specifies that `Person` objects will have two instance variables: `name` and `age`.
* `classVariableNames: ''` specifies that `Person` will not have any class variables.

2. Adding Methods to the `Person` Class:

```smalltalk
Person class methods: [
  getName
  getAge
  setName:
  setAge:
]

Person instance methods: [
  name
  age
  setName:
  setAge:
]
```

* `class methods:` and `instance methods:` are used to define class methods and instance methods, respectively.
* `getName` and `getAge` are class methods that return the `name` and `age` of a `Person` object, respectively.
* `setName:` and `setAge:` are class methods that set the `name` and `age` of a `Person` object, respectively.
* `name` and `age` are instance methods that return the `name` and `age` of a `Person` object, respectively.
* `setName:` and `setAge:` are instance methods that set the `name` and `age` of a `Person` object, respectively.

3. Creating a `Person` Object:

```smalltalk
person := Person new.
```

* `Person new` creates a new instance of the `Person` class and assigns it to the variable `person`.

4. Setting the Name and Age of the `Person` Object:

```smalltalk
person setName: 'John Doe'.
person setAge: 30.
```

* `setName:` sets the `name` of the `person` object to 'John Doe'.
* `setAge:` sets the `age` of the `person` object to 30.

5. Getting the Name and Age of the `Person` Object:

```smalltalk
name := person name.
age := person age.
```

* `name` gets the `name` of the `person` object and assigns it to the variable `name`.
* `age` gets the `age` of the `person` object and assigns it to the variable `age`.

6. Printing the Name and Age of the `Person` Object:

```smalltalk
Transcript show: name; cr.
Transcript show: age; cr.
```

* `Transcript show:` prints the value of `name` to the Transcript window.
* `cr` moves the cursor to the next line.
* `Transcript show:` prints the value of `age` to the Transcript window.
* `cr` moves the cursor to the next line.

Output:

```
John Doe
30
```