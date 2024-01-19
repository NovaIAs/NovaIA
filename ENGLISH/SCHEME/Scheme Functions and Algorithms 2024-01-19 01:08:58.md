```scheme
(define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))

(define (fibonacci n)
    (cond
        [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (lcm a b)
    (* a (/ b (gcd a b))))

(define (prime? n)
    (cond
        [(= n 1) false]
        [(= n 2) true]
        [else (and
                 (null? (filter
                           (lambda (d) (= 0 (remainder n d)))
                           (range 2 (floor (sqrt n)))))
                 (= n (car (cdr (filter
                                     (lambda (d) (= 0 (remainder n d)))
                                     (range 2 (floor (sqrt n))))))))])))

(define (sieve n)
    (let loop ((primes (list 2)))
        (if (empty? primes)
            primes
            (apply append
                   (cons (filter prime? (range (+ 3 (* (caar primes) 2)) (+ 2 n) 2))
                         (loop (filter (lambda (x) (not (= (remainder x (caar primes)) 0))) primes))))))

(define (partition n lst)
    (if (empty? lst)
        '()
        (let ((sublst (take n lst)))
            (cons sublst
                  (partition n (drop n lst))))))

(define (transpose lst)
    (if (empty? lst)
        '()
        (let ((first-row (map car lst)))
            (cons first-row
                  (transpose (map cdr lst))))))

(define (matrix-multiply A B)
    (let loop ((C (make-matrix (length A) (length (car B)))))
        (for-each (row A)
            (for-each (col B)
                (set-matrix-element! C (add1 (matrix-row C row)) (add1 (matrix-col C col))
                                    (+ (matrix-ref C row col) (* (matrix-ref A row col) (matrix-ref B row col))))))
        C))

(define (solve-linear-system A b)
    (let loop ((piv-col 0) (piv-row 0))
        (if (and (= piv-row (length A)) (= piv-col (length (car A))))
            (error "No unique solution")
            (if (zero? (matrix-ref A piv-row piv-col))
                (if (and (= piv-col (length A)) (= piv-row (add1 (length A)))))
                    (values '() (false))
                    (loop piv-col (add1 piv-row)))
                (let ((scale (/ (matrix-ref b piv-row) (matrix-ref A piv-row piv-col))))
                    (set-matrix-element! A piv-row piv-col 1)
                    (for-each (i (range 0 (length A)))
                        (if (not (= i piv-row))
                            (set-matrix-element! A i piv-col 0)
                            (set-matrix-element! b i (- (matrix-ref b i) (* (matrix-ref A i piv-col) scale)))))
                    (set-matrix-element! b piv-row scale)
                    (loop (add1 piv-col) piv-row))))))
```