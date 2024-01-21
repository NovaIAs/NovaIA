```scheme
(define (factorial n)
    (if (zero? n)
        1
        (* n (factorial (sub1 n)))))

(define (fibonacci n)
    (if (< n 2)
        n
        (+ (fibonacci (sub1 n)) (fibonacci (sub2 n)))))

(define (gcd a b)
    (if (zero? b)
        a
        (gcd b (remainder a b))))

(define (lcm a b)
    (* a b) (gcd a b))

(define (is-prime? n)
    (if (<= n 1)
        false
        (and (zero? (remainder n 2)) (zero? (remainder n 3)) (zero? (remainder n 5)) (zero? (remainder n 7))))

(define (prime-factors n)
    (define (prime-factors-helper n primes)
        (if (is-prime? n)
            (cons n primes)
            (prime-factors-helper (quotient n (car primes)) (cdr primes))))
    (prime-factors-helper n '()))

(define (count-primes n)
    (define (count-primes-helper n primes)
        (if (<= n 1)
            0
            (+ (is-prime? n) (count-primes-helper (sub1 n) primes))))
    (count-primes-helper n '()))

(define (sieve-of-eratosthenes n)
    (define (sieve-of-eratosthenes-helper n marked)
        (if (>= n 2)
            (sieve-of-eratosthenes-helper (sub1 n) (cons (zero? (car marked)) (cdr marked))))
            marked))
    (sieve-of-eratosthenes-helper n (make-vector n #t)))

(define (is-palindrome? s)
    (define (is-palindrome?-helper s start end)
        (if (<= start end)
            (and (char=? (string-ref s start) (string-ref s end)) (is-palindrome?-helper s (add1 start) (sub1 end)))
            true))
    (is-palindrome?-helper s 0 (sub1 (string-length s))))

(define (reverse-string s)
    (string-append (reverse s) ""))

(define (substring s start end)
    (substring s start (sub1 end)))

(define (find-string s substring)
    (define (find-string-helper s substring start)
        (if (>= start (string-length s))
            -1
            (if (string=? (substring substring start (add1 start)) substring)
                start
                (find-string-helper s substring (add1 start)))))
    (find-string-helper s substring 0))

(define (replace-string s old-substring new-substring)
    (define (replace-string-helper s old-substring new-substring start)
        (if (>= start (string-length s))
            ""
            (if (string=? (substring s start (add start (string-length old-substring))) old-substring)
                (string-append new-substring (replace-string-helper s old-substring new-substring (add start (string-length old-substring))))
                (string-append (substring s start (add1 start)) (replace-string-helper s old-substring new-substring (add1 start))))))
    (replace-string-helper s old-substring new-substring 0))

(define (split-string s delimiter)
    (define (split-string-helper s delimiter start)
        (if (>= start (string-length s))
            '()
            (if (string=? (substring s start (add1 start)) delimiter)
                (cons (substring s 0 start) (split-string-helper s delimiter (add1 start)))
                (split-string-helper s delimiter (add1 start)))))
    (split-string-helper s delimiter 0))

(define (join-string strings)
    (define (join-string-helper strings result)
        (if (null? strings)
            result
            (join-string-helper (cdr strings) (string-append result (car strings)))))
    (join-string-helper strings ""))

(define (remove-duplicates l)
    (define (remove-duplicates-helper l result)
        (if (null? l)
            result
            (if (not (member (car l) result))
                (remove-duplicates-helper (cdr l) (cons (car l) result))
                (remove-duplicates-helper (cdr l) result))))
    (remove-duplicates-helper l '()))

(define (sort l)
    (define (sort-helper l result)
        (if (null? l)
            result
            (sort-helper (cdr l) (insert (car l) result))))
    (sort-helper l '()))

(define (insert x l)
    (if (null? l)
        (list x)
        (if (> x (car l))
            (cons x l)
            (cons (car l) (insert x (cdr l))))))

(define (min l)
    (define (min-helper l minValue)
        (if (null? l)
            minValue
            (if (< (car l) minValue)
                (min-helper (cdr l) (car l))
                (min-helper (cdr l) minValue))))
    (min-helper l (car l)))

(define (max l)
    (define (max-helper l maxValue)
        (if (null? l)
            maxValue
            (if (> (car l) maxValue)
                (max-helper (cdr l) (car l))
                (max-helper (cdr l) maxValue))))
    (max-helper l (car l)))

(define (sum l)
    (define (sum-helper l total)
        (if (null? l)
            total
            (sum-helper (cdr l) (+ (car l) total))))
    (sum-helper l 0))

(define (product l)
    (define (product-helper l total)
        (if (null? l)
            total
            (product-helper (cdr l) (* (car l) total))))
    (product-helper l 1))

(define (average l)
    (/ (sum l) (length l)))

(define (median l)
```