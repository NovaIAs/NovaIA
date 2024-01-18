```scheme
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (% a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (prime? n)
  (if (= n 1)
    false
    (let loop ((i 2))
      (if (or (>= i n) (= (remainder n i) 0))
        false
        (loop (+ i 1))))))

(define (sieve n)
  (let loop ((i 2) (primes '()))
    (if (>= i n)
      primes
      (if (prime? i)
        (loop (+ i 1) (cons i primes))
        (loop (+ i 1) primes)))))

(define (goldbach? n)
  (if (even? n)
    (let loop ((i 2))
      (if (or (>= i n) (prime? i) (prime? (- n i)))
        false
        (loop (+ i 1))))
    false))

(define (partition n k)
  (let* ((result '()) (candidates (iota n)))
    (loop
      (when (= k 0)
        (cons candidates result))
      (if (null? candidates)
        result
        (let ((head (car candidates)))
          (loop
            (set! candidates (cdr candidates))
            (set! result (cons (cons head (partition (- n 1) (- k 1))) result)))))))))

(define (change n)
  (let ((result '()))
    (loop
      (when (<= n 0)
        result)
      (if (>= n 10)
        (loop (- n 10) (cons 10 result))
        (loop (- n 5) (cons 5 result))
        (loop (- n 1) (cons 1 result))))))

(define (make-change n)
  (let ((ways (vector (make-vector (+ n 1) '0))))
    (vector-set! ways 0 1)
    (loop
      (when (= n 0)
        (vector-ref ways 0))
      (let ((i 1))
        (loop
          (when (= i (1+ n))
            (let ((sum 0))
              (loop
                (when (>= i 0)
                  (set! sum (+ sum (vector-ref ways (- n i))))
                  (set! i (- i 1))))
              (vector-set! ways (+ n 1) sum)))
          (when (<= i (1+ n))
            (set! i (+ i 1)))))))))

(define (hanoi n)
  (let ((result '()))
    (loop
      (when (= n 0)
        result)
      (let ((from "A") (to "B") (via "C") (temp (list '())))
        (loop
          (when (null? temp)
            (cons (list from to) result))
          (if (null? (car temp))
            (set! temp (cons (list from via) temp))
            (if (null? (cdr temp))
              (set! temp (cons (list via to) temp))
              (set! temp (cdr temp))))))
        (loop
          (when (= n 0)
            result)
          (let ((from "B") (to "A") (via "C"))
            (loop
              (when (null? temp)
                (cons (list from to) result))
              (if (null? (car temp))
                (set! temp (cons (list from via) temp))
                (if (null? (cdr temp))
                  (set! temp (cons (list via to) temp))
                  (set! temp (cdr temp)))))))))
        (loop
          (when (= n 0)
            result)
          (let ((from "C") (to "A") (via "B"))
            (loop
              (when (null? temp)
                (cons (list from to) result))
              (if (null? (car temp))
                (set! temp (cons (list from via) temp))
                (if (null? (cdr temp))
                  (set! temp (cons (list via to) temp))
                  (set! temp (cdr temp)))))))))
        (loop
          (when (= n 0)
            result)
          (let ((from "A") (to "C") (via "B"))
            (loop
              (when (null? temp)
                (cons (list from to) result))
              (if (null? (car temp))
                (set! temp (cons (list from via) temp))
                (if (null? (cdr temp))
                  (set! temp (cons (list via to) temp))
                  (set! temp (cdr temp)))))))))
        (loop
          (when (= n 0)
            result)
          (let ((from "B") (to "C") (via "A"))
            (loop
              (when (null? temp)
                (cons (list from to) result))
              (if (null? (car temp))
                (set! temp (cons (list from via) temp))
                (if (null? (cdr temp))
                  (set! temp (cons (list via to) temp))
                  (set! temp (cdr temp)))))))))
        (loop
          (when (= n 0)
            result)
          (let ((from "C") (to "B") (via "A"))
            (loop
              (when (null? temp)
                (cons (list from to) result))
              (if (null? (car temp))
                (set! temp (cons (list from via) temp))
                (if (null? (cdr temp))
                  (set! temp (cons (list via to) temp))
                  (set! temp (cdr temp)))))))))
        (set! n (- n 1))