```smalltalk
Object subclass: #MyClass [
    instanceVariableNames: 'instanceVariable'

    classVariableNames: 'classVariable'

    poolDictionaries: ''

    category: 'Examples'
]

MyClass class >> initialize [
    super initialize.
    self classVariable := 42.
]

MyClass class >> classVariable [
    ^ classVariable.
]

MyClass class >> setClassVariable: aValue [
    self classVariable := aValue.
]

MyClass new >> instanceVariable [
    ^ instanceVariable.
]

MyClass new >> setInstanceVariable: aValue [
    self instanceVariable := aValue.
]

MyClass new >> classVariable [
    ^ self class >> classVariable.
]

MyClass new >> setClassVariable: aValue [
    self class >> setClassVariable: aValue.
]
```

This code creates a new class called `MyClass` in Smalltalk. The `initialize` method is called when a new instance of `MyClass` is created. In this method, the class variable `classVariable` is initialized to the value `42`.

The `classVariable` method returns the value of the class variable `classVariable`. The `setClassVariable:` method sets the value of the class variable `classVariable` to the given value.

The `instanceVariable` method returns the value of the instance variable `instanceVariable`. The `setInstanceVariable:` method sets the value of the instance variable `instanceVariable` to the given value.

The `classVariable` and `setClassVariable:` methods can also be used on instances of `MyClass` to access and set the class variable `classVariable`.

This code demonstrates the use of class variables and instance variables in Smalltalk. Class variables are shared among all instances of a class, while instance variables are unique to each instance of a class.