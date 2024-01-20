```scheme
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (lcm a b)
  (* a (/ b (gcd a b))))

(define (prime? n)
  (if (<= n 1)
      #f
      (let loop ((d 2))
        (if (>= d (sqrt n))
            #t
            (if (= 0 (remainder n d))
                #f
                (loop (+ d 1)))))))

(define (sieve-of-eratosthenes n)
  (let loop ((primes (list)))
    (if (<= (+ (car primes) 1) n)
        (let loop2 ((i (+ (car primes) 1)) (not-primes (list)))
          (if (> i n)
              (reverse (cons (car primes) primes))
              (if (not (member i not-primes))
                  (loop2 (+ i (car primes)) (cons i not-primes))
                  (loop2 (+ i 1) not-primes))))))
```

Este código implementa tres algoritmos fundamentales, factoriales, Fibonacci y el máximo común divisor/mínimo común múltiplo, usando funciones auxiliares como esPrimo y criba de Eratóstenes.