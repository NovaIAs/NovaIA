```scheme
(define (make-complex-code)
  (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))

  (define (fibonacci n)
    (if (or (= n 0) (= n 1))
        n
        (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

  (define (lcm a b)
    (* a b) (/ (gcd a b))))

  (define (prime? n)
    (for*/and ([i 2]
              [j (/ n i)])
      (not (= (remainder n i) 0))))

  (define (merge xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [(< (car xs) (car ys))
       (cons (car xs) (merge (cdr xs) ys))]

      [else
       (cons (car ys) (merge xs (cdr ys)))]))

  (define (quicksort xs)
    (cond
      [(null? xs) '()]
      [(= (length xs) 1) (list (car xs))]

      [else
       (let* ([pivot (car xs)]
              [left (filter (lambda (x) (< x pivot)) (cdr xs))]
              [right (filter (lambda (x) (>= x pivot)) (cdr xs))])
         (append (quicksort left)
                 (list pivot)
                 (quicksort right)))]))

  (define (heapsort xs)
    (let* ([n (length xs)]
           [h xs])
      (for ([i (- n 1)]
          [j 0])
        (cond
          [(not (< j i)) h]
          [(< (h j) (h (/ (+ j 1) 2)))
           (set! (h j) (h (/ (+ j 1) 2)))
           (set! (h (/ (+ j 1) 2)) (h j))]

          [else
           (set! (h j) (h i))
           (set! (h i) (list-ref h (/ (+ j 1) 2)))
           (set! (h (/ (+ j 1) 2)) (list-ref h j)))]))

      [(for ([j 0]
          [i (- n 1)])
        (when (>= j (/ n 2))
          (list-set! h j (list-ref h i))))
       '()]

      h))

  (define (make-graph n)
    (let* ([g (make-list n)])
       (for/and ([i 0]
               [u (in-range 0 (- n 1))])
         (set! (list-ref g i) '())))))

  (define (add-edge g u v w)
    (list-push! (list-ref g u) (cons v w)))

  (define (dijkstra g s t)
    (let* ([n (length g)]
           [dist (make-vector n)]
           [visited (make-vector n)])
       (for [i (in-range 0 (- n 1))]
         (set! (vector-ref dist i) inf))
       (set! (vector-ref dist s) 0)
       (set! (vector-ref visited s) true)
       (let loop ([v s]
                  [u]
                  [w]
                  [min-dist inf])
         (for [i (in-range n)]
           (if (and (not (vector-ref visited i))
                    (< (vector-ref dist i) min-dist))
               (begin
                 (set! u i)
                 (set! w min-dist)
                 (set! min-dist (vector-ref dist i))
               )))
         (when (< min-dist inf)
           (for*/dolist ([a (list-ref g u)])
             (let* ([v (car a)]
                    [w (cdr a)])
               (cond
                 [(< (+ min-dist w) (vector-ref dist v))]
                  (set! (vector-ref dist v) (+ min-dist w)))
                 [else '()]])
             (set! (vector-ref visited u) true)
             (loop v))))))

  (define (bellman-ford g s)
    (let* ([n (length g)]
           [dist (make-vector n)]
           [parent (make-vector n)])
       (for [i (in-range n)]
         (set! (vector-ref dist i) inf))
       (set! (vector-ref dist s) 0)
       (for [i (in-range 1 n)]
         (let loop ([u (in-range 0 (- n 1))])
            (for*/dolist ([a (list-ref g u)])
              (let* ([v (car a)]
