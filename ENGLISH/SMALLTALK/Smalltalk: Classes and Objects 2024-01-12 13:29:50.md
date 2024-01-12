```smalltalk
Class hierarchy:

Object
  | instanceVariables |
  new
    ^ super new

  initialize
    | instanceVariableNames |
    instanceVariableNames := self class instanceVariableNames.
    instanceVariableNames do: [:instanceVariableName |
      self instanceVariableNamed: instanceVariableName value: nil].
    ^ self

  instanceVariableNamed: instanceVariableName value: value
    | offset |
    offset := self class offsetOfInstanceVariable: instanceVariableName.
    self basicAt: offset put: value

  valueForInstanceVariableNamed: instanceVariableName
    | offset |
    offset := self class offsetOfInstanceVariable: instanceVariableName.
    ^ self basicAt: offset

Class
  | superclass instanceVariableNames |
  new: superclass instanceVariableNames: instanceVariableNames
    ^ super new.
    subclass: superclass.
    instanceVariableNames := instanceVariableNames.

  superclass
    ^ superclass

  instanceVariableNames
    ^ instanceVariableNames

  offsetOfInstanceVariable: instanceVariableName
    | offset |
    offset := 0.
    instanceVariableNames do: [:ivName |
      offset := offset + 4.
      [ivName = instanceVariableName] whileFalse: [^ offset]].
    ^ -1 "not found"

Object subclass: superclass instanceVariableNames: instanceVariableNames
  ^ self new: superclass instanceVariableNames: instanceVariableNames

Object subclass: superclass
  ^ self subclass: superclass instanceVariableNames: {}

Object subclass: superclass instanceVariableNames: instanceVariableNames

  | newClass |
  newClass := Class new: superclass instanceVariableNames: instanceVariableNames.
  superclass subclasses add: newClass.
  ^ newClass

Examples:

(Object subclass: #Rectangle instanceVariableNames: {#x #y #width #height}) new x: 10 y: 20 width: 30 height: 40

(Object subclass: #Rectangle instanceVariableNames: {#x #y #width #height}) new
  x: 10;
  y: 20;
  width: 30;
  height: 40

Explanation:

The first example creates a new class called Rectangle that inherits from the Object class. The Rectangle class has four instance variables: x, y, width, and height. The new method of the Rectangle class creates a new Rectangle object and initializes its instance variables to the specified values.

The second example is equivalent to the first example, but it uses a more concise syntax. The x:, y:, width:, and height: methods are used to set the values of the corresponding instance variables.

The Class class is a metaclass that represents classes in Smalltalk. The new method of the Class class creates a new class. The superclass and instanceVariableNames arguments specify the superclass of the new class and the names of its instance variables.

The subclass method of the Object class is used to create a new subclass of the Object class. The superclass and instanceVariableNames arguments specify the superclass of the new subclass and the names of its instance variables.

The subclasses method of the Class class returns a collection of the subclasses of the receiver.

The add: method of the Collection class adds an object to the collection.
```