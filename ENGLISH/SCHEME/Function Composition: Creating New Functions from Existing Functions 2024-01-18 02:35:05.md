```scheme
(define make-adder
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define add1 (make-adder 1))
(define add2 (make-adder 2))

(add1 3)  ; 4
(add2 3)  ; 5
```

This code defines a function called `make-adder` that takes a number as an argument and returns a function that adds that number to any other number.

For example, the following code defines a function called `add1` that adds 1 to any number:

```scheme
(define add1 (make-adder 1))
```

And the following code defines a function called `add2` that adds 2 to any number:

```scheme
(define add2 (make-adder 2))
```

We can then use these functions to add numbers together. For example, the following code adds 3 to 1:

```scheme
(add1 3)  ; 4
```

And the following code adds 3 to 2:

```scheme
(add2 3)  ; 5
```

This code is a good example of how functions can be used to create new functions. In this case, the `make-adder` function is used to create the `add1` and `add2` functions. This makes it easy to add new functions to a program without having to rewrite the entire program.

Here is a more complex example of how functions can be used to create new functions:

```scheme
(define (make-factorial-function)
  (let loop
    ((n 1)
     (factorial 1))
    (cons n
          (lambda (x)
            (loop (+ n 1)
                  (* factorial x))))))

(define factorial (make-factorial-function))

(factorial 5)  ; 120
```

This code defines a function called `make-factorial-function` that returns a function that calculates the factorial of a number. The factorial of a number is the product of all the positive integers less than or equal to that number. For example, the factorial of 5 is 120, because 5! = 5 * 4 * 3 * 2 * 1 = 120.

The `make-factorial-function` function uses a recursive let expression to define a function called `loop`. The `loop` function takes two arguments: `n`, which is the current number, and `factorial`, which is the factorial of `n`. The `loop` function returns a cons cell that contains `n` and a function that calls the `loop` function again with `n+1` and `* factorial x`.

The `factorial` function is defined as the result of calling the `make-factorial-function` function.

We can then use the `factorial` function to calculate the factorial of any number. For example, the following code calculates the factorial of 5:

```scheme
(factorial 5)  ; 120
```

This code is a good example of how functions can be used to create recursive functions. In this case, the `make-factorial-function` function is used to create the `factorial` function, which is a recursive function that calculates the factorial of a number.