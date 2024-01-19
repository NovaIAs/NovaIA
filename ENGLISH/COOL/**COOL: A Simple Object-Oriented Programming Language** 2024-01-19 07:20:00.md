```cool
-- Define classes, classes will be compiled to objects
Class Int {
  -- Class fields, they are not stored directly into objects, they are loaded
  -- dynamically at run time, classes are open objects.
  field next: Int;
  -- Methods are defined here
  method new(value: Int): Int {
    next := nil;
    return self;
  }
  -- Methods have expressions, expressions should be typechecked
  method value(): Int {
    return self;
  }
  method nextValue(): Int {
    if isvoid(next) then next := new(0);
    return next;
  }
  method printValue(): String {
    return nextValue().value().toString();
  }
}
Class String {
  field value: String;
  method new(value: String): String {
    return self;
  }
  method value(): String {
    return value;
  }
  method length(): Int {
    return value.length();
  }
  method printValue(): String {
    return "\"" + value + "\"";
  }
}
-- Define methods in base classes
String.new("Cool").printValue();
Object.abort("The end of program.");

-- Define the main method
Class Main {
  method main(): Int {
    let x : Int <- new(10); // x is a reference to an object of class Int
    x.printValue();
    x.nextValue().printValue();
    x.nextValue().printValue();
    x.nextValue().printValue();
    x.nextValue().printValue();
    return 0;
  }
}
```

**Explanation:**

* The code defines two classes, `Int` and `String`, with their respective fields and methods.
* The `Int` class has a field `next` that points to another `Int` object, and methods to create new `Int` objects, access the value of an `Int` object, access the value of the next `Int` object, and print the value of an `Int` object.
* The `String` class has a field `value` that stores the string value, and methods to create new `String` objects, access the value of a `String` object, get the length of a `String` object, and print the value of a `String` object.
* The `Main` class defines the `main` method, which is the entry point of the program.
* In the `main` method, an object of class `Int` is created and assigned to the variable `x`.
* The methods `printValue`, `nextValue`, and `nextValue` are called on the object `x` to print the values of the current object, the next object, and the next object's next object.
* The `abort` method of the `Object` class is called to end the program.

This code is a simple example of how to define classes and methods in COOL, and how to create and use objects.