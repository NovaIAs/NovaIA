```scheme

(define (modulo-counter max)
  (let loop ((count 0))
    (if (= count max)
        0
        (modulo (+ count 1) max))))

(define (fibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibonacci (sub1 n)) (fibonacci (sub1 (sub1 n)))))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (sub1 n)))))

(define (min-max-sum a b)
  (list (min a b) (max a b) (+ a b)))

(define (chain-of-lists lst)
  (if (null? lst)
      '()
      (append (list (car lst)) (chain-of-lists (cdr lst)))))

(define (merge-lists lst)
  (if (null? lst)
      '()
      (let ((head (car lst))
            (tail (cdr lst)))
        (append (map cons head (merge-lists tail)) (merge-lists tail)))))

(define (reverse-list lst)
  (if (null? lst)
      '()
      (append (reverse-list (cdr lst)) (list (car lst)))))

(define (split-list lst n)
  (if (null? lst)
      '()
      (if (= (length lst) n)
          lst
          (append (list (car lst)) (split-list (cdr lst) n)))))

(define (dup-adj-list lst)
  (map (lambda (x) (list x x)) lst))

(define (find-min-max lst)
  (fold-left (lambda (x y) (min x y)) (max-value) lst))

(define (sum-of-squares lst)
  (fold-left + 0 (map square lst)))

(define (every? pred lst)
  (if (null? lst)
      #t
      (and (pred (car lst)) (every? pred (cdr lst)))))

(define (some? pred lst)
  (if (null? lst)
      #f
      (or (pred (car lst)) (some? pred (cdr lst)))))

(define (member? x lst)
  (if (null? lst)
      #f
      (or (= x (car lst)) (member? x (cdr lst)))))

(define (remove x lst)
  (filter (lambda (y) (not (= x y))) lst))

(define (unique-elements lst)
  (sort (remove-duplicates lst)))

(define (combinations n lst)
  (if (= n 0)
      (list '())
      (let ((smaller (combinations (sub1 n) lst)))
        (map (lambda (x) (cons (car lst) x)) smaller))))

(define (permutations lst)
  (if (null? lst)
      (list '())
      (let ((smaller (permutations (cdr lst)))
        (map (lambda (x) (insert (car lst) x)) smaller))))

(define (insert x lst)
  (let loop ((lst lst) (prev '()))
    (if (null? lst)
        (append prev (list x))
        (if (<= (car lst) x)
            (append prev (list (car lst)))
            (append prev (list x (car lst))))
        (loop (cdr lst) (cons (car lst) prev)))))