```cool
class MyClass {
    a: Int;
    b: String;
    c: Bool;

    constructor(a: Int, b: String, c: Bool) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    method1(x: Int, y: String) {
        if (this.c) {
            return this.a + x;
        } else {
            return this.b + y;
        }
    }

    method2(x: Int, y: String) {
        while (x > 0) {
            this.a = this.a + 1;
            x = x - 1;
        }

        while (y != "") {
            this.b = this.b + y[0];
            y = y[1..];
        }
    }

    method3(x: Int, y: String) {
        for (i in 0..x) {
            this.a = this.a + i;
        }

        for (i in 0..y.length) {
            this.b = this.b + y[i];
        }
    }

    method4(x: Int, y: String) {
        case x of
            0 => this.a = 0;
            1 => this.a = 1;
            2 => this.a = 2;
            else => this.a = 3;

        case y of
            "a" => this.b = "a";
            "b" => this.b = "b";
            "c" => this.b = "c";
            else => this.b = "d";
    }
}
```

This code defines a class called `MyClass` with three instance variables: `a` (an integer), `b` (a string), and `c` (a boolean).

The class has a constructor that takes three arguments and initializes the instance variables to the values of the arguments.

The class also has four methods:

* `method1` takes two arguments, `x` (an integer) and `y` (a string), and returns the value of `a` plus `x` if `c` is true, or the value of `b` plus `y` if `c` is false.
* `method2` takes two arguments, `x` (an integer) and `y` (a string), and increments the value of `a` by `x` and concatenates the value of `b` with the value of `y`.
* `method3` takes two arguments, `x` (an integer) and `y` (a string), and adds the values of `a` and `i` for each value of `i` in the range from 0 to `x`, and concatenates the value of `b` with the value of `y[i]` for each value of `i` in the range from 0 to the length of `y`.
* `method4` takes two arguments, `x` (an integer) and `y` (a string), and assigns the value of `x` to `a` and the value of `y` to `b` depending on the value of `x` and `y`.