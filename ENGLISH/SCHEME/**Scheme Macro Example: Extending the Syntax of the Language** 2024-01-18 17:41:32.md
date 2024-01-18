```scheme
(define (my-macro x y)
  (let ((z (+ x y)))
    (display z)
    (newline)))

(my-macro 10 20)

(define-syntax my-syntax
  (syntax-rules ()
    ((_ x y) (my-macro x y))))

(my-syntax 30 40)
```

**Explanation:**

1. Define a macro called `my-macro` that takes two arguments, `x` and `y`.

2. Inside the macro, use `let` to define a local variable `z` and assign it the value of `(+ x y)`.

3. Use `display` to print the value of `z` to the console.

4. Use `newline` to print a newline to the console.

5. Define a syntax rule for the macro `my-syntax`. This rule matches any expression that starts with `_` followed by two arguments, `x` and `y`.

6. When the macro `my-syntax` is called, it expands to the expression `(my-macro x y)`, which calls the `my-macro` macro with the arguments `x` and `y`.

7. Call the macro `my-syntax` with the arguments `30` and `40`. This expands to the expression `(my-macro 30 40)`, which calls the `my-macro` macro with the arguments `30` and `40`.

8. The `my-macro` macro prints the value of `z` (which is 70) to the console and then prints a newline.

9. The result of the macro call is printed to the console, which is `70`.