```cool
class MyComplexClass {
    public :=
        a:    Int;
        b:    String;
        c:    Array[Int];
        d:    Bool;
        e:    MyOtherClass;
        f:    (Int -> String);
        g:    ((String, Int) -> Bool);
    end;
    initialize :=
        a := 1;
        b := "Hello";
        c := [1, 2, 3];
        d := true;
        e := new MyOtherClass();
        f := (x) => { "The integer is " + x.to_string(); };
        g := (s, x) => { s.length() > x; };
    end;
    print :=
        print(a);
        print(", ");
        print(b);
        print(", ");
        print(c);
        print(", ");
        print(d);
        print(", ");
        e.print();
        print(", ");
        print(f(5));
        print(", ");
        print(g("Hello, world", 10));
    end;
};

class MyOtherClass {
    public :=
        x:   Int;
        y:   String;
    end;
    initialize :=
        x := 4;
        y := "World";
    end;
    print :=
        print(x);
        print(", ");
        print(y);
    end;
};
```

This COOL program defines two classes, `MyComplexClass` and `MyOtherClass`.

The `MyComplexClass` class has the following instance variables and methods.

* `a`: An integer variable.
* `b`: A string variable.
* `c`: An array of integers.
* `d`: A boolean variable.
* `e`: An instance of the `MyOtherClass` class.
* `f`: A method that takes an integer as input and returns a string.
* `g`: A method that takes two arguments, a string and an integer, and returns a boolean.
* `initialize`: The constructor method for the `MyComplexClass` class.
* `print`: A method that prints the values of the instance variables.

The `MyOtherClass` class has the following instance variables and methods.

* `x`: An integer variable.
* `y`: A string variable.
* `initialize`: The constructor method for the `MyOtherClass` class.
* `print`: A method that prints the values of the instance variables.

Here is an example of how the `MyComplexClass` class can be used.

```cool
var x: MyComplexClass := new MyComplexClass();
x.print();
```

This code will create a new `MyComplexClass` object and call the `print` method on it. The output of the program will be:

```
1, Hello, [1, 2, 3], true, 4, World, The integer is 5, true
```

Here is an explanation of the code.

* The `main` method creates a new `MyComplexClass` object and calls the `print` method on it.
* The `initialize` method is the constructor method for the `MyComplexClass` class. It initializes the instance variables to their default values.
* The `print` method prints the values of the instance variables.
* The `f` method is a method that takes an integer as input and returns a string. It uses the `to_string` method to convert the integer to a string.
* The `g` method is a method that takes two arguments, a string and an integer, and returns a boolean. It compares the length of the string to the integer and returns true if the length of the string is greater than the integer.