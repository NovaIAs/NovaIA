```cool
class Main is
    main() : Object =>
        if (true) then
            (
                let n : Int <- 1000;
                let sum : Int <- 0;
                while (n > 0) loop
                    sum <- sum + n;
                    n <- n - 1
                pool;
                sum
            )
        else
            0
        fi
pool;

class Int is
    + (x : Int) : Int =>
        x + self;
    - (x : Int) : Int =>
        self - x;
    < (x : Int) : Bool =>
        self < x;
    = (x : Int) : Bool =>
        self = x;
pool;

class Bool is
    & (x : Bool) : Bool =>
        self and x;
    | (x : Bool) : Bool =>
        self or x;
    not() : Bool =>
        not self
pool;

class While is
    loop(x : Bool) : Object =>
        while (x) loop
            x <- self.body()
        pool;
        self.exit()
pool;

class Let is
    in(x : Object) : Object =>
        let self.id : self.type <- self.value;
        x
pool;
```

This code implements a simple loop in COOL. The `Main` class defines a `main` method that returns an `Object`. The `main` method uses an `if` statement to check if the condition `true` is true. If it is, the `main` method uses a `let` statement to declare a variable `n` of type `Int` and initialize it to 1000. The `main` method also declares a variable `sum` of type `Int` and initializes it to 0. The `main` method then uses a `while` loop to iterate over the values from 1000 to 0. Inside the loop, the `main` method adds the value of `n` to `sum` and decrements `n` by 1. After the loop, the `main` method returns the value of `sum`. If the condition `true` is false, the `main` method returns 0.

The `Int` class defines the `+`, `-`, `<`, and `=` methods. The `+` method adds two `Int`s together and returns the result. The `-` method subtracts one `Int` from another and returns the result. The `<` method compares two `Int`s and returns true if the first `Int` is less than the second `Int`. The `=` method compares two `Int`s and returns true if the two `Int`s are equal.

The `Bool` class defines the `&`, `|`, and `not` methods. The `&` method performs a logical AND operation on two `Bool`s and returns the result. The `|` method performs a logical OR operation on two `Bool`s and returns the result. The `not` method negates a `Bool` and returns the result.

The `While` class defines the `loop` method. The `loop` method takes a `Bool` as an argument and returns an `Object`. The `loop` method executes the body of the loop until the condition `x` is false. The body of the loop is defined by the `body` method. The `exit` method is called when the loop terminates.

The `Let` class defines the `in` method. The `in` method takes an `Object` as an argument and returns an `Object`. The `in` method declares a variable `id` of type `self.type` and initializes it to the value of `self.value`. The `in` method then executes the body of the let statement and returns the result.