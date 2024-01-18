```scheme
(define-syntax <$>
  (syntax-rules ()
    ((_ . expr)
     (list 'lambda (x) (list (car expr) x)))))

(define-syntax <$>fl
  (syntax-rules ()
    ((_ . expr)
     (list 'lambda (x) (list (first expr) x))))))

(define-syntax <$g$f
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$f$g args))
                              (list (car expr) h)))))))

(define-syntax <$>l
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (cons x args)))))))

(define-syntax <$:$
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (car args)))))))

(define-syntax <$:$%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list (first expr) (first args) (cons (car rest) (cdr rest)))))))

(define-syntax <$:$%f
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <fl$g rest))
                              (list (first expr) (first args) (h)))))))

(define-syntax <$:%
  (syntax-rules ()
    (((_ args . rest) . expr)
     (list 'lambda (x) (list (first expr) (cons x (cdr args)) (first rest)))))))

(define-syntax <$:%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list (first expr) (cons 'do (cdr args)) (first rest)))))))

(define-syntax <$:%%f
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (first args)))))))

(define-syntax <$>f%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (list 'do (cons (car expr) (cdr args))) (car args)))))))

(define-syntax <$>f%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (list 'do (cdr args)))))))

(define-syntax <$:%h
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (list 'do (cons (car expr) (cdr args))) (cons (car args) (cdr args)))))))

(define-syntax <$:%h%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (list 'do (cons (car expr) (cdr args))) (cons (car args) (cdr args))))))

(define-syntax <$>f:%h
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (list 'do (cons (car expr) (cdr args))) (list (car args) (cdr args)))))))

(define-syntax <$>f:%h%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (list 'do (cons (car expr) (cdr args))) (list (car args) (cdr args))))))

(define-syntax <$:%h:%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$g$f rest))
                              (list (first expr) (h) (list 'do (cons (first expr) (cdr args))) (cons (first args) (cdr args)))))))

(define-syntax <$>f:%h:%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (x) (list (first expr) (list 'do (cons (car expr) (cdr args))) (list (first args) (cdr args)))))))

(define-syntax <$>f:%h:%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (list 'do (cons (first expr) (cdr args))) (list (first args) (cdr args)))))))

(define-syntax <$:$h:%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list (first expr) (list 'do (cons (first expr) (cons (car args) (cdr args)))) (first rest))))))

(define-syntax <$:$h%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (x) (list 'do
                              ((h <$>fl$g rest))
                              (list (first expr) (h) (list 'do (cons (first expr) (cons (car args) (cdr args)))) (first rest))))))

(define-syntax <$>fl$g
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (car expr)))
                              (append (list l) (cdr expr)))))))

(define-syntax <$f$g
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr expr)))))))

(define-syntax <$>l$g
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr expr)))))))

(define-syntax <$l$g
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr expr)))))))

(define-syntax <$:%l$g
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons (car rest) (cdr rest)))
                              (cdr expr)))))))

(define-syntax <$:%l$g%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons (car rest) (cdr rest))))
                              (cdr expr))))))

(define-syntax <$:%l%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do (cdr args)))
                              (cdr expr))))))

(define-syntax <$:$%h$g
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cons (car rest) (cdr rest)))))
                              (cdr expr)))))))

(define-syntax <$:$%h%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cons (car rest) (cdr rest)))))
                              (cdr expr))))))

(define-syntax <$:%h$g
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (car expr) (cdr args)))))
                              (cdr expr)))))))

(define-syntax <$:%h%%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (car expr) (cdr args)))))
                              (cdr expr))))))

(define-syntax <$>f$g%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (car expr)))
                              (append (list l) (list 'do (cdr args)))
                              (cdr expr)))))))

(define-syntax <$>f$g%%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (car expr)))
                              (append (list l) (list 'do (cdr args)))
                              (cdr expr))))))

(define-syntax <$:%f$g
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr args))
                              (cdr expr)))))))

(define-syntax <$:%f$g%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr args))
                              (cdr expr))))))

(define-syntax <$:%f%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (list 'do (cdr args)))
                              (cdr expr))))))

(define-syntax <$:%f%%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (list 'do (cdr args)))
                              (cdr expr))))))

(define-syntax <$:%h:%h
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cdr args))))
                              (cons 'do
                                 (list (first expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$:%h:%h%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cdr args))))
                              (cons 'do
                                 (list (first expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$:$h:%h
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (first args) (cdr args)))))
                              (cons 'do
                                 (list (first expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$:$h:%h%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (first args) (cdr args)))))
                              (cons 'do
                                 (list (first expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$>f:%h:%h
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cdr args))))
                              (cons 'do
                                 (list (car expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$>f:%h:%h%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (first args) (cdr args))))
                              (cons 'do
                                 (list (car expr) (cdr args)))
                              (cdr expr)))))))

(define-syntax <$:$%:%h
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons (car rest) (cdr rest)))
                              (cdr expr)))))))

(define-syntax <$:$%:%h%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons (car rest) (cdr rest)))
                              (cdr expr))))))

(define-syntax <$:%:%h
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr args))
                              (cdr expr)))))))

(define-syntax <$:%:%h%
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr args))
                              (cdr expr))))))

(define-syntax <$:$%:%l
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (car rest) (cdr rest)))))
                              (cdr expr)))))))

(define-syntax <$:$%:%l%
  (syntax-rules ()
    (((_ rest . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cons 'do
                                                 (list (cons (car rest) (cdr rest)))))
                              (cdr expr))))))

(define-syntax <$:%%:%l
  (syntax-rules ()
    (((_ . args) . expr)
     (list 'lambda (l) (list 'let ((h (first expr)))
                              (append (list l) (cdr args))
