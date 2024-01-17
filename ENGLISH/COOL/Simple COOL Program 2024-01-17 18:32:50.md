```cool
class Object {
    new : -> (self) {.self};
}

class Main extends Object {
    main : -> (self) {
        let a : Int <- 10;
        if a < 10 then
            let b : Int <- 20;
        else
            let b : Int <- 30;
        fi;
        let c : Int <- 40;
        while c > 0 loop
            let c : Int <- c - 1;
        pool;
        let d : Int <- 50;
        return d;
    };
}
```

This code is a complete program written in COOL, a general-purpose programming language designed for educational purposes. The program defines a class called `Object` and a class called `Main` that inherits from `Object`. The `Main` class defines a method called `main` which is the entry point of the program.

The `main` method first declares an integer variable `a` and assigns it the value 10. Then, it checks if `a` is less than 10. If it is, then it declares another integer variable `b` and assigns it the value 20. Otherwise, it declares `b` and assigns it the value 30.

Next, the `main` method declares an integer variable `c` and assigns it the value 40. Then, it starts a while loop that will execute as long as `c` is greater than 0. Inside the loop, it decrements `c` by 1.

After the loop, the `main` method declares an integer variable `d` and assigns it the value 50. Finally, it returns `d`.

The following is a breakdown of the code:

* The `Object` class is a simple class that defines a method called `new` which returns a new instance of the class.
* The `Main` class inherits from the `Object` class and defines a method called `main` which is the entry point of the program.
* The `main` method first declares an integer variable `a` and assigns it the value 10.
* The `main` method then checks if `a` is less than 10. If it is, then it declares another integer variable `b` and assigns it the value 20. Otherwise, it declares `b` and assigns it the value 30.
* The `main` method next declares an integer variable `c` and assigns it the value 40.
* The `main` method then starts a while loop that will execute as long as `c` is greater than 0. Inside the loop, it decrements `c` by 1.
* After the loop, the `main` method declares an integer variable `d` and assigns it the value 50.
* Finally, the `main` method returns `d`.

This code is a simple example of a COOL program. It demonstrates the use of variables, control flow statements, and classes.