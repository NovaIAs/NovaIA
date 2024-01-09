```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (prime? n)
  (and (> n 1)
       (every? (lambda (d) (not (= (remainder n d) 0)))
               (range 2 (sqrt n)))))

(define (next-prime n)
  (let loop ((i (+ n 1)))
    (if (prime? i)
        i
        (loop i))))

(define (sieve n)
  (let loop ((i 2)
             (primes '()))
    (if (> i n)
        primes
        (if (prime? i)
            (loop (+ i 1) (cons i primes))
            (loop (+ i 1) primes)))))

(define (nth-prime n)
  (car (take n (sieve 100000))))

(define (factors n)
  (let loop ((n n)
             (factors '()))
    (if (= n 1)
        factors
        (let ((p (next-prime n)))
          (if (= (remainder n p) 0)
              (loop (/ n p) (cons p factors))
              (loop n factors))))))

(define (divisors n)
  (let loop ((factors (factors n))
             (divisors '()))
    (if (null? factors)
        divisors
        (let ((p (car factors))
              (rest (cdr factors)))
          (append divisors
                 (append (map (lambda (e) (* p e)) (divisors n))
                        (divisors (/ n p)))))))

(define (perfect-number? n)
  (and (> n 0)
       (= n (apply + (divisors n)))))

(define (abundant-number? n)
  (and (> n 0)
       (> (apply + (divisors n)) n)))

(define (deficient-number? n)
  (and (> n 0)
       (< (apply + (divisors n)) n)))

(define (amicable-pair? a b)
  (and (> a 0)
       (> b 0)
       (= a (apply + (divisors b)))
       (= b (apply + (divisors a)))))

(define (sum-of-proper-divisors n)
  (apply + (divisors n)))

(define (sum-of-divisors n)
  (+ n (apply + (divisors n))))

(define (sum-of-squares n)
  (apply + (map square (range 1 n))))

(define (sum-of-cubes n)
  (apply + (map cube (range 1 n))))

(define (sum-of-factorials n)
  (apply + (map factorial (range 1 n))))

(define (sum-of-reciprocals n)
  (apply + (map (lambda (x) (/ 1 x)) (range 1 n))))

(define (harmonic-mean n)
  (/ n (sum-of-reciprocals n))))

(define (geometric-mean n)
  (expt (apply * (range 1 n)) (/ 1 n))))

(define (arithmetic-mean n)
  (/ (apply + (range 1 n)) n))))

(define (median n)
  (let ((sorted (sort n)))
    (if (even? (length sorted))
        (/ (+ (car (take (/ (length sorted) 2) sorted))
               (car (drop (/ (length sorted) 2) sorted))) 2)
        (car (take (/ (length sorted) 2) sorted)))))

(define (mode n)
  (let ((frequencies (frequencies n)))
    (car (filter (lambda (pair) (= (cdr pair) (max-value frequencies))) frequencies))))

(define (range a b)
  (if (<= a b)
      (cons a (range (+ a 1) b))
      '()))

(define (take n xs)
  (if (or (null? xs) (= n 0))
      '()
      (cons (car xs) (take (- n 1) (cdr xs)))))

(define (drop n xs)
  (if (or (null? xs) (= n 0))
      xs
      (drop (- n 1) (cdr xs))))

(define (sort xs)
  (if (null? xs)
      '()
      (let ((pivot (car xs))
            (lesser (filter (lambda (x) (< x pivot)) (cdr xs)))
            (greater (filter (lambda (x) (>= x pivot)) (cdr xs))))
        (append (sort lesser) (cons pivot (sort greater))))))

(define (frequencies xs)
  (map (lambda (x) (cons x (length (filter (lambda (y) (= x y)) xs)))) xs))

(define (max-value xs)
  (apply max xs))

(define (min-value xs)
  (apply min xs))
```