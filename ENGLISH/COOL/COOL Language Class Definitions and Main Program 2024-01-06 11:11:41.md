```cool

-- Superclass Object
class Object {
  -- method abort: () -> Void
  abort(): Self.abort;
  -- method copy: () -> Object
  copy(): Self.copy;
  -- method type_name: () -> String
  type_name(): String;
}

-- Class IO
class IO extends Object {
  -- method out_string: (String) -> Void
  out_string(x: String): Self;
  -- method out_int: (Int) -> Void
  out_int(x: Int): Self;
  -- method in_string: () -> String
  in_string(): String;
  -- method in_int: () -> Int
  in_int(): Int;
}

-- Class String
class String extends Object {
  -- method length: () -> Int
  length(): Int;
  -- method concat: (String) -> String
  concat(s: String): String;
  -- method substr: (Int, Int) -> String
  substr(i: Int, l: Int): String;
}

-- Class Int
class Int extends Object {
  -- method +: (Int) -> Int
  +(y: Int): Int;
  -- method -: (Int) -> Int
  -(y: Int): Int;
  -- method *: (Int) -> Int
  *(y: Int): Int;
  -- method /: (Int) -> Int
  /(y: Int): Int;
  -- method neg: () -> Int
  neg(): Int;
}

-- Class Bool
class Bool extends Object {
  -- method not: () -> Bool
  not(): Bool;
  -- method and: (Bool) -> Bool
  and(x: Bool): Bool;
  -- method or: (Bool) -> Bool
  or(x: Bool): Bool;
}

-- Class Array
class Array extends Object {
  -- method new: (Int, Object) -> Array
  new(size: Int, init: Object): Self;
  -- method length: () -> Int
  length(): Int;
  -- method get: (Int) -> Object
  get(i: Int): Object;
  -- method set: (Int, Object) -> Void
  set(i: Int, y: Object): Self;
}

-- Class Main
class Main extends Object {
  -- method main: () -> Void
  main(): Self {
    var x: Int;
    x := 42;
    (IO.new()).out_int(x);
    (IO.new()).out_string("\n");
    0;
  }
}

```

Explanation:

This code defines several classes in the COOL programming language, including the base class Object, and classes for input/output (IO), strings (String), integers (Int), booleans (Bool), and arrays (Array). The Main class contains the main method, which prints the integer 42 to the console using the out_int method of the IO class.

Here's a breakdown of each class and its methods:

1. Object:
   - abort(): Terminates the current program.
   - copy(): Creates a copy of the current object.
   - type_name(): Returns the name of the class of the object.

2. IO:
   - out_string(x): Prints the string x to the console.
   - out_int(x): Prints the integer x to the console.
   - in_string(): Reads a line of text from the console and returns it as a string.
   - in_int(): Reads an integer from the console and returns it.

3. String:
   - length(): Returns the length of the string.
   - concat(s): Concatenates the string s to the current string and returns the result.
   - substr(i, l): Returns a substring of the current string starting at index i and of length l.

4. Int:
   - +(y): Adds the integer y to the current integer and returns the result.
   - -(y): Subtracts the integer y from the current integer and returns the result.
   - *(y): Multiplies the integer y by the current integer and returns the result.
   - /(y): Divides the current integer by the integer y and returns the result.
   - neg(): Negates the current integer and returns the result.

5. Bool:
   - not(): Returns the negation of the current boolean value.
   - and(x): Performs a logical AND operation between the current boolean value and the boolean value x and returns the result.
   - or(x): Performs a logical OR operation between the current boolean value and the boolean value x and returns the result.

6. Array:
   - new(size, init): Creates a new array of size size and initializes each element with the value init.
   - length(): Returns the length of the array.
   - get(i): Returns the element at index i in the array.
   - set(i, y): Sets the element at index i in the array to the value y.

7. Main:
   - main(): The entry point of the program. It creates an integer variable x, sets it to 42, prints it to the console using the out_int method of the IO class, and then prints a newline using the out_string method.