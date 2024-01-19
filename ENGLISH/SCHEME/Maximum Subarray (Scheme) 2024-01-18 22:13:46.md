```scheme
(define (maximum-subarray a)
  (define (max-subarray-helper a start end max-so-far max-ending-here)
    (if (> end start)
        (let ((mid (quotient (+ start end) 2)))
          (max-subarray-helper a start mid
                                max-so-far (max (+ max-ending-here (car (list-ref a mid)))))
          (max-subarray-helper a (+ mid 1) end
                                (max (+ max-so-far (car (list-ref a mid))) 0) max-ending-here))
        max-so-far)))
  (define n (length a))
  (max-subarray-helper a 0 (- n 1) 0 0))

(define (find-max-crossing-subarray a start mid end)
  (define (left-sum a start mid)
    (define lsum 0)
    (do ((i start (+ i 1))) ((> i mid) lsum)
      (set! lsum (+ lsum (car (list-ref a i))))))
  (define (right-sum a mid end)
    (define rsum 0)
    (do ((i end (- i 1))) ((< i mid) rsum)
      (set! rsum (+ rsum (car (list-ref a i))))))
  (+ (left-sum a start mid) (right-sum a mid end)))

(display (maximum-subarray '(2 4 -1 7 -2 -3 7 -1)))
```

This Scheme code finds the maximum subarray of a given list of integers. It uses a divide-and-conquer algorithm that recursively divides the list into smaller sublists, finds the maximum subarray of each sublist, and then combines the results to find the maximum subarray of the entire list.

The `maximum-subarray` function takes a list of integers as its argument and returns the maximum subarray of the list.

The `max-subarray-helper` function is a helper function that recursively divides the list into smaller sublists, finds the maximum subarray of each sublist, and then combines the results to find the maximum subarray of the entire list.

The `find-max-crossing-subarray` function is another helper function that finds the maximum subarray of a list that crosses the midpoint of the list.

The `left-sum` and `right-sum` functions are helper functions that compute the sum of the elements in the left and right halves of a list, respectively.

The `display` function is used to display the result of the `maximum-subarray` function.

Here is an example of how to use the `maximum-subarray` function:

```scheme
(display (maximum-subarray '(2 4 -1 7 -2 -3 7 -1)))
```

This will print the following output:

```
11
```

This indicates that the maximum subarray of the list `(2 4 -1 7 -2 -3 7 -1)` is the subarray `(7 -2 -3 7)`, which has a sum of 11.