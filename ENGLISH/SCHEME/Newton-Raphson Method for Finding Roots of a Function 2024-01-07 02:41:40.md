```scheme
(define (newton-raphson f df x)
  (define (newton-step x)
    (- x (/ (f x) (df x))))
  (if (close-enough? x (newton-step x))
      x
      (newton-raphson f df (newton-step x))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (f x)
  (- (* x x) 2))

(define (df x)
  (* 2 x))

(display (newton-raphson f df 1))
```

This code implements the Newton-Raphson method for finding the roots of a function. The code takes three arguments:

* `f`: The function whose roots we want to find.
* `df`: The derivative of the function `f`.
* `x`: An initial guess for the root of the function.

The code first defines a helper function `newton-step` that takes a single argument `x` and returns the next guess for the root of the function. The next guess is calculated by subtracting the value of the function at `x` divided by the value of the derivative of the function at `x` from `x`.

The code then defines a helper function `close-enough?` that takes two arguments `x` and `y` and returns `#t` if `x` and `y` are close enough to each other, and `#f` otherwise. Two numbers are considered close enough if the absolute value of their difference is less than 0.0001.

The code then defines the function `f` that we want to find the roots of. In this case, the function is `f(x) = x^2 - 2`.

The code then defines the derivative of the function `f`. In this case, the derivative is `df(x) = 2x`.

The code then displays the result of calling the `newton-raphson` function with the arguments `f`, `df`, and `1`. The result is the root of the function `f` that is closest to the initial guess of `1`.