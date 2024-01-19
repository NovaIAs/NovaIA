```scheme
(define (fact n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
    n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (% a b))))

(define (lcm a b)
  (* (/ a (gcd a b)) b))

(define (primo? n)
  (if (= n 1)
    false
    (every? (lambda (d) (not (= (% n d) 0)))
            (range 2 (sqrt n))) ))

(define (sumatoria f a b)
  (if (> a b)
    0
    (+ (f a) (sumatoria f (+ a 1) b))))

(define (producto f a b)
  (if (> a b)
    1
    (* (f a) (producto f (+ a 1) b))))

(define (mapcar f lst)
  (if (null? lst)
    '()
    (cons (f (car lst)) (mapcar f (cdr lst)))))

(define (filter f lst)
  (if (null? lst)
    '()
    (if (f (car lst))
      (cons (car lst) (filter f (cdr lst)))
      (filter f (cdr lst)))))

(define (foldr f init lst)
  (if (null? lst)
    init
    (f (car lst) (foldr f init (cdr lst)))))

(define (foldl f init lst)
  (if (null? lst)
    init
    (foldl f (f (car lst) init) (cdr lst))))

```