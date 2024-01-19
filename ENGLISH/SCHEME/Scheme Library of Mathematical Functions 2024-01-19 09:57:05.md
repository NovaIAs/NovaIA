```scheme

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (gcd a b)
  (cond ((= b 0) a)
        ((> b a) (gcd b a))
        (else (gcd (- a b) b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (is-prime? n)
  (cond ((= n 1) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else (for/list ([i (in-range 3 (sqrt n) 2)])
                 (not (= (remainder n i) 0))))))

(define (prime-factors n)
  (cond ((is-prime? n) (list n))
        (else (let loop ([n n] [factors '()])
                 (cond ((even? n) (loop (/ n 2) (cons 2 factors)))
                       ((is-prime? n) (cons n factors))
                       (else (let ([p (find-prime-factor n)])
                                (loop (/ n p) (cons p factors)))))))))

(define (find-prime-factor n)
  (for/list ([i (in-range 3 (sqrt n) 2)])
             (when (= (remainder n i) 0) i)))

(define (partitions n)
  (cond ((= n 0) '(()))
        (else (let loop ([n n] [partitions '()])
                 (for/list ([i (in-range 1 n)])
                           (loop (- n i) (cons (list i) partitions)))))))

(define (combinations n r)
  (if (>= r n)
    '()
    (let loop ([n n] [r r] [combinations '()])
         (if (= r 0)
           combinations
           (let ([next (- n 1)])
             (loop next (- r 1) (cons (list n) combinations))))))))

(define (fibonacci n)
  (if (<= n 1)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (ackermann m n)
  (cond ((= m 0) (add1 n))
        ((and (= n 0) (> m 0)) 0)
        (else (ackermann (- m 1) (ackermann m (- n 1))))))

(define (add1 n)
  (+ n 1))

```