```scheme
(define (factorial n)
  (if (zero? n)
    1
    (* n (factorial (- n 1)))))

(define (suma-vector v)
  (if (empty? v)
    0
    (+ (car v) (suma-vector (cdr v)))))

(define (max-vector v)
  (if (empty? v)
    0
    (max (car v) (max-vector (cdr v)))))

(define (min-vector v)
  (if (empty? v)
    0
    (min (car v) (min-vector (cdr v)))))

(define (media-vector v)
  (/ (suma-vector v) (length v)))

(define (mediana-vector v)
  (let ([ordenado (sort v)])
    (if (even? (length v))
      (/ (+ (car (cdr ordenado)) (car (cddr ordenado))) 2)
      (car (cddr ordenado)))))

(define (moda-vector v)
  (let ([tabla (make-hash)]
    (for-each (lambda (x)
                (hash-set! tabla x
                           (incf (hash-ref tabla x) 0))) v))
    (let* ([max-frecuencia (max (hash-values tabla))]
           [moda '()])
      (for-each (lambda (x)
                  (if (= (hash-ref tabla x) max-frecuencia)
                    (cons x moda)))
                (hash-keys tabla))
      moda)))

(define (desviacion-tipica v)
  (let ([media (media-vector v)])
    (sqrt (1.0 / (- (length v) 1))
          (suma-vector (mapcar (lambda (x)
                                 (sqr (- x media))) v)))))

(define (varianza v)
  (sqr (desviacion-tipica v)))

(define (covarianza v1 v2)
  (/ (suma-vector (mapcar (lambda (x y)
                             (* (- x (media-vector v1))
                                (- y (media-vector v2))))
                           v1 v2))
     (- (length v1) 1)))

(define (correlacion v1 v2)
  (/ (covarianza v1 v2)
     (* (desviacion-tipica v1)
        (desviacion-tipica v2))))

(define (recta-regresion v1 v2)
  (let* ([cov (covarianza v1 v2)]
         [var (varianza v1)]
         [m (/ cov var)]
         [b (- (media-vector v2)
                (* m (media-vector v1))))]
    (list m b)))

(define (rango v)
  (- (max-vector v) (min-vector v)))

(define (cuartil-inferior v)
  (let ([ordenado (sort v)])
    (/ (min-vector (sublist ordenado
                          0 (- (length ordenado) 2))) 2)))

(define (cuartil-superior v)
  (let ([ordenado (sort v)])
    (/ (max-vector (sublist ordenado
                          (+ (length ordenado) 2)
                          (length ordenado)))) 2)))

(define (intercuartil v)
  (- (cuartil-superior v) (cuartil-inferior v)))

(define (boxplot-cuartiles v)
  (list (min-vector v)
        (cuartil-inferior v)
        (media-vector v)
        (cuartil-superior v)
        (max-vector v)))

(define (quintil-inferior v)
  (let ([ordenado (sort v)])
    (/ (min-vector (sublist ordenado
                          0 (- (length ordenado) 4))) 2)))

(define (quintil-superior v)
  (let ([ordenado (sort v)])
    (/ (max-vector (sublist ordenado
                          (+ (length ordenado) 4)
                          (length ordenado)))) 2)))

(define (interquintil v)
  (- (quintil-superior v) (quintil-inferior v)))

(define (boxplot-quintiles v)
  (list (min-vector v)
        (quintil-inferior v)
        (media-vector v)
        (quintil-superior v)
        (max-vector v)))

(define (decil-inferior v)
  (let ([ordenado (sort v)])
    (/ (min-vector (sublist ordenado
                          0 (- (length ordenado) 9))) 2)))

(define (decil-superior v)
  (let ([ordenado (sort v)])
    (/ (max-vector (sublist ordenado
                          (+ (length ordenado) 9)
                          (length ordenado)))) 2)))

(define (interdecil