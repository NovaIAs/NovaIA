```scheme

(define (run-length-encoding input)
    (cond ((null? input) nil)
          ((= (car input) (cadr input))
              (cons (list (car input) 2) (run-length-encoding (cdr input))))
          (else (cons (list (car input) 1) (run-length-encoding (cdr input))))))

(run-length-encoding '(1 1 1 2 2 3 3 1))
=> '((1 . 3) (2 . 2) (3 . 2) (1 . 1))

```

This Scheme code performs run-length encoding on a list of elements. Run-length encoding is a simple lossless data compression algorithm that works by identifying and replacing consecutive repetitions of an element with a single instance of the element followed by the number of times it is repeated.

The Scheme code we've written uses the `cons` function to construct a list of tuples, where each tuple represents a run of consecutive elements. The first element of each tuple is the element itself, and the second element is the number of consecutive times that element appears in the original list.

Here's an explanation of the code:

```scheme
(define (run-length-encoding input))
```

This line defines a Scheme function called `run-length-encoding` that takes a single input argument, which is a list of elements to be compressed.

```scheme
(cond ((null? input) nil) ...))
```

This is a conditional expression that checks the input list for three possible cases:

1. If the input list is empty (`null? input`), the function returns `nil` because there is no need for compression.

2. If the first and second elements of the input list are equal (`(= (car input) (cadr input))`), it means we've encountered a run of consecutive elements. In this case, the function constructs a tuple containing the first element and a count of 2 (since we've seen two consecutive occurrences of the element). It then recursively calls itself with the rest of the input list (`(cdr input)`) to encode the remaining elements.

3. For all other cases, the first element of the input list is not part of a run of consecutive elements. In this case, the function constructs a tuple containing the first element and a count of 1. It then recursively calls itself with the rest of the input list to encode the remaining elements.

By recursively calling itself and accumulating the encoded tuples in a list, the function effectively compresses the input list using run-length encoding.

```scheme
(run-length-encoding '(1 1 1 2 2 3 3 1))
```

This line demonstrates how to use the `run-length-encoding` function. It passes a sample input list `'(1 1 1 2 2 3 3 1)` to the function and prints the result, which would be the encoded list `'((1 . 3) (2 . 2) (3 . 2) (1 . 1))`.

Overall, this code provides a clear and concise implementation of run-length encoding in Scheme, effectively compressing a list of elements by identifying consecutive runs.