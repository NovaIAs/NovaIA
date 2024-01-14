```scheme
(define (find-maximum-subarray a)
  (let loop ((max-so-far 0)
             (max-ending-here 0)
             (start 0)
             (end 0))
    (if (= end (length a))
        (values max-so-far start end)
        (let ((new-max-ending-here (+ max-ending-here (car a))))
          (if (> new-max-ending-here 0)
              (loop max-so-far new-max-ending-here start end)
              (loop 0 0 (1+ end) (1+ end))))))))

(define (maximum-subarray-sum a)
  (let ((subarray (find-maximum-subarray a)))
    (apply + (subarray-ref a (car subarray) (cdr subarray)))))

(define (test-maximum-subarray-sum)
  (display (maximum-subarray-sum '(5 1 4 -2 3 1 -3 2 5))))
```

Explanation:

This Scheme code defines a function called `maximum-subarray-sum` that finds the maximum sum of a contiguous subarray within a given list of numbers `a`. It uses a dynamic programming approach to efficiently solve this problem. Here's how the code works:

1. The `find-maximum-subarray` function takes a list `a` as its argument and returns a triple containing:
   - `max-so-far`: The maximum sum of a contiguous subarray found so far.
   - `start`: The starting index of the maximum subarray.
   - `end`: The ending index of the maximum subarray.

2. The `loop` function is a recursive helper function that implements the dynamic programming algorithm. It takes four arguments:
   - `max-so-far`: The maximum sum of a contiguous subarray seen so far in the current iteration.
   - `max-ending-here`: The maximum sum of a contiguous subarray ending at the current index.
   - `start`: The starting index of the current maximum subarray.
   - `end`: The ending index of the current maximum subarray.

3. The `loop` function iterates through the elements of the list `a` and updates the values of `max-so-far`, `max-ending-here`, `start`, and `end` based on the current element.

4. If the current element is positive, it is added to `max-ending-here`, indicating that including this element in the current subarray increases the sum. Otherwise, `max-ending-here` is reset to 0, starting a new subarray.

5. If the current `max-ending-here` value is greater than `max-so-far`, it means a new maximum subarray has been found, and the values of `max-so-far`, `start`, and `end` are updated accordingly.

6. The loop continues until the end of the list is reached, at which point it returns the values of `max-so-far`, `start`, and `end`, representing the maximum subarray.

7. The `maximum-subarray-sum` function calls `find-maximum-subarray` to find the maximum subarray and then uses `apply` and `subarray-ref` to sum the elements of the maximum subarray, returning the maximum subarray sum.

8. The `test-maximum-subarray-sum` function is a test function that demonstrates the usage of `maximum-subarray-sum`. It prints the maximum subarray sum for a sample list of numbers.