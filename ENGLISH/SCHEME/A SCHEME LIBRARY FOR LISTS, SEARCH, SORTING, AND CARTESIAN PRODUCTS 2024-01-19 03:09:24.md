```scheme
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fibonacci (- n 1))
             (fibonacci (- n 2)))]))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (lcm a b)
  (/ (* a b) (gcd a b)))

(define (isPrime n)
  (and (>= n 2)
       (null? (filter (lambda (d) (zero? (remainder n d)))
                     (range 2 (sqrt n))))))

(define (primeFactors n)
  (define (factors n d)
    (if (= n 1)
        '(d)
        (let loop ((n n) (d (+ d 1)))
          (cond
            [(zero? (remainder n d))
             (cons d (factors (/ n d) d))]
            [else (loop n d)])))))

  (factors n 2))

(define (ackermann m n)
  (if (= m 0)
      (+ n 1)
      (if (= n 0)
          (ackermann (- m 1) 1)
          (ackermann (- m 1) (ackermann m (- n 1))))))

(define (quicksort xs)
  (cond
    [(empty? xs) '()]
    [(= (length xs) 1) xs]
    [else (let ((pivot (first xs)))
           (let ((smaller (filter (lambda (x) (< x pivot)) (rest xs)))
                 (larger (filter (lambda (x) (>= x pivot)) (rest xs))))
             (append (quicksort smaller) (cons pivot) (quicksort larger))))]))

(define (mergesort xs)
  (cond
    [(empty? xs) '()]
    [(= (length xs) 1) xs]
    [else (let ((mid (quotient (length xs) 2)))
           (let ((left (take mid xs))
                 (right (drop mid xs)))
             (merge (mergesort left) (mergesort right))))]))

(define (heapsort xs)
  (define (heapify xs i)
    (let loop ((xs xs) (i i) (n (length xs)))
      (cond
        [(zero? i) xs]
        [(< (let* ((left (* 2 i))
                     (right (+ (* 2 i) 1)))
              (when (>= left n)
                (zero? left))
              (when (>= right n)
                (zero? right))
              (>= (nth xs i)
                  (max (nth xs left) (nth xs right)))) true)]
           (values xs i))
        [else (let* ((largest (max (nth xs i)
                                     (nth xs left)
                                     (nth xs right))))
               (set-car! xs largest)
               (set-car! xs (nth xs right))
               (set-car! xs (nth xs left)))
           (loop (values xs (- i 1) n))))))

  (define (buildHeap xs)
    (let n (length xs))
      (loop (xs xs) (let i (- n 1))
        (when (>= i 0)
          (set-car! xs (heapify xs i))))))

  (define (heapsort! xs)
    (begin
      (buildHeap xs)
      (let n (- (length xs) 1))
        (loop (xs xs)
          (when (> n 0)
            (let ((temp (first xs)))
              (set-car! xs (nth xs n))
              (set-car! xs temp)
              (set-car! xs (heapify xs 0 n)))))))))

(define (binarySearch xs x)
  (let rec loop (a b)
    (if (>= b a)
        (let ((mid (quotient (+ a b) 2)))
          (cond
            [(> (nth xs mid) x) (loop a (- mid 1))]
            [(< (nth xs mid) x) (loop (+ mid 1) b)]
            [else mid]))
        -1)))

(define (insertionSort xs)
  (let rec loop (ys x)
    (cond
      [(empty? ys) '(x)]
      [(<= x (first ys)) (cons x ys)]
      [else (let ((ys (rest ys)))
             (cons (first ys) (loop ys x)))])))

  (let ((sorted '()))
    (for-each (lambda (x) (set-car! sorted (loop sorted x))) xs)))

(define (selectionSort xs)
  (let rec loop (sorted unsorted)
    (cond
      [(empty? unsorted) sorted]
      [else (let ((min (min-list unsorted)))
             (let ((sorted (cons min sorted)))
               (loop sorted (remove min unsorted))))]))

  (loop '() xs))

(define (bubbleSort xs)
  (let rec loop (xs)
    (cond
      [(empty? xs) '()]
      [(= 1 (length xs)) xs]
      [else (let ((i 1) (j (length xs)) (sorted? true))
             (let loop2 ()
               (cond
                 [(= i j) (if sorted? sorted? (loop (rest xs)))]
                 [(> (nth xs (- i 1)) (nth xs i))
                  (begin (swap! (vector xs (- i 1)) (vector xs i))
                        (set-car! sorted? false))
                  (loop2)]
                 [else (set! i (+ i 1)) (loop2)])))
             (loop2))))))

  (loop xs))

(define (merge xs ys)
  (let rec loop (a b)
    (cond
      [(empty? a) b]
      [(empty? b) a]
      [(<= (first a) (first b))
       (cons (first a) (loop (rest a) b))]
      [else (cons (first b) (loop a (rest b)))])))

  (loop xs ys))

(define (reverse xs)
  (reverse xs))

(define (map f xs)
  (for-each f xs))

(define (filter f xs)
  (let rec loop (xs ys)
    (cond
      [(empty? xs) ys]
      [(f (first xs)) (loop (rest xs) (cons (first xs) ys))]
      [else (loop (rest xs) ys)])))

  (loop xs '()))

(define (reduce f x xs)
  (cond
    [(empty? xs) x]
    [else (reduce f (f x (first xs)) (rest xs))]))

(define (scan f x xs)
  (let rec loop (ys z)
    (cond
      [(empty? xs) ys]
      [else (loop (cons (f z (first xs)) ys) (f z (first xs)))])))

  (loop '() x))

(define (for-each f xs)
  (cond
    [(empty? xs) '()]
    [else (cons (f (first xs)) (for-each f (rest xs)))]))

(define (unique xs)
  (let rec loop (ys zs)
    (cond
      [(empty? xs) ys]
      [(or (member (first xs) ys) (member (first xs) zs))
       (loop ys (cons (first xs) zs))]
      [else (loop (cons (first xs) ys) zs)])))

  (loop '() '()))

(define (union xs ys)
  (let rec loop (a b)
    (cond
      [(empty? a) b]
      [(empty? b) a]
      [(< (first a) (first b))
       (cons (first a) (loop (rest a) b))]
      [else (cons (first b) (loop a (rest b)))])))

  (loop xs ys))

(define (intersection xs ys)
  (let rec loop (a b)
    (cond
      [(empty? a) '()]
      [(empty? b) '()]
      [(= (first a) (first b))
       (cons (first a) (loop (rest a) (rest b)))]
      [(< (first a) (first b))
       (loop a (rest b))]
      [else (loop (rest a) b)])))

  (loop xs ys))

(define (difference xs ys)
  (let rec loop (a b)
    (cond
      [(empty? a) '()]
      [(empty? b) a]
      [(= (first a) (first b))
       (loop (rest a) (rest b))]
      [(< (first a) (first b))
       (cons (first a) (loop (rest a) b))]
      [else (loop a (rest b)))])))

  (loop xs ys))

(define (symmetricDifference xs ys)
  (union (difference xs ys) (difference ys xs)))

(define (cartesianProduct xs ys)
  (for-each (lambda (x) (map (lambda (y) (list x y)) ys)) xs))

(define (permutations xs)
  (let rec loop (xs ys)
    (cond
      [(empty? xs) '(ys)]
      [else (let ((ys (list (first xs) ys)))
             (let ((zs (loop (rest xs) ys)))
               (append zs (for-each (lambda (z) (loop (rest xs) z)) ys)))))]))

  (loop xs '()))

(define (combinations xs n)
  (let rec loop (xs ys)
    (cond
      [(empty? xs) '()]
      [(= n 1) (map (lambda (x) (list x)) xs)]
      [else (let ((zs (loop (rest xs) ys)))
             (append zs (for-each (lambda (z) (loop (rest xs) (cons (first xs) z))) ys)))))]))

  (loop xs '()))