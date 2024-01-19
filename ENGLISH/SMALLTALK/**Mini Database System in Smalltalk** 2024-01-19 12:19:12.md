**Objective:**
Create a comprehensive data management system that mimics a mini database application in Smalltalk.

**Code:**

```smalltalk
**Class definition for Object Store**
ObjectStore subclass: DataManager [
    instance variables: allObjects
    class variables: schema
].

**Message to create new object store**
DataManager new [
    | schema |
    schema := Dictionary new.
    ^ super new setAllObjects: Dictionary new.
].

**Message to retrieve information about object**
DataManager >> infoForObject: anObject [
    | name type |
    name := anObject class name.
    type := schema at: name ifAbsent: [schema at: name put: self describeObject: anObject].
    ^ type.
].

**Message to add new class to schema**
DataManager >> extendSchemaForClass: aClass [
    schema at: aClass class name put: aClass dictionary.
].

**Message to describe object structure**
DataManager >> describeObject: anObject [
    | attributes |
    attributes := anObject class attributes.
    Dictionary keysAndValues: [
        attributes do: [:attr | [attr name, attr type]].
    ].
].

**Class definition for basic object**
DBObject subclass: BasicObject [
    instance variables: id creationDate modificationDate
    class variables: schema idCounter
].

**Message to initialize new object**
BasicObject new [
    | self now |
    now := DateAndTime now.
    super new.
    id := idCounter := idCounter + 1.
    creationDate := now.
    modificationDate := now.
    ^ self.
].

**Message to create object of a given type**
DataManager >> createObjectOfType: aClass [
    ^ aClass new.
].

**Message to find all objects of a given type**
DataManager >> findObjectsOfType: aClass [
    | result |
    result := self allObjects keys select: [:key | aClass isSuperclassOf: key class].
    result do: [:key | [key, self allObjects at: key]].
].

**Message to delete an object from the store**
DataManager >> deleteObject: anObject [
    self allObjects removeKey: anObject.
].

**Message to display all objects in a store**
DataManager >> printAllObjects [
    self allObjects keysAndValues do: [:key :value | [
        Transcript show: key name, ' -> ';
        Transcript show: value asJSON;
        Transcript cr.
    ]].
].

**Message to save the object store to a file**
DataManager >> saveToFile: aFileName [
    File write: aFileName with: self asJSON.
].

**Message to load the object store from a file**
DataManager >> loadFromFile: aFileName [
    self allObjects := JSON readFromFile: aFileName asDictionary.
].
```

**Explanation:**

* The `DataManager` class defines the main object store and provides methods to manage and manipulate objects.
* The `BasicObject` class is a base class for all objects stored in the system. It provides basic properties like `id`, `creationDate`, and `modificationDate`.
* The `extendSchemaForClass:` method adds a new class to the schema, describing its structure.
* The `createObjectOfType:` method creates a new object of a specified type.
* The `findObjectsOfType:` method finds all objects of a specified type.
* The `deleteObject:` method deletes an object from the store.
* The `printAllObjects` method prints all objects in the store to the transcript.
* The `saveToFile:` method saves the object store to a file in JSON format.
* The `loadFromFile:` method loads the object store from a file in JSON format.

This code provides a comprehensive object management system that can be used to create, retrieve, update, and delete objects of different types. It also allows for saving and loading the object store to and from files.