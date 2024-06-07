**Module de calcul polynomial**

```scheme
(define-syntax convert-poly
  (syntax-rules ()
    ((_ literal)
     (list 'quote literal)
     (syntax-error "literal attendu")
    ((_ (constant) ...)
     (list 'make-polynomial (cadr (syntax-quote constant))
           (list 'constant (cdr (syntax-quote constant))))
     (syntax-error "constante attendue")
    ((_ (variable) ...)
     (list 'make-polynomial (cadr (syntax-quote variable))
           (list 'variable (cdr (syntax-quote variable))))
     (syntax-error "variable attendue")
    ((_ (operation operand1 operand2 ...) ...)
     (list 'make-polynomial (cadr (syntax-quote operation))
           (append '() (for-each convert-poly (list operand1 operand2 ...))))
     (syntax-error "opération attendue")
    ((_)
     (syntax-error "syntaxe incorrecte")))))

(define (differentiate poly)
  (match (syntax->datum poly)
    [(make-polynomial _ (list 'constant s))
     (make-polynomial (current-fraction)
       (list 'constant s))]
    [(make-polynomial _ (list 'variable x))
     (make-polynomial -
       (list 'constant (current-fraction))
       (list 'variable x))]
    [(make-polynomial _ (list 'operation op args ...))
     (cond [(eq? op '+)
            (make-polynomial +
              (for-each differentiate args ...))]
           [(eq? op '-')
            (make-polynomial -
              (for-each differentiate args ...))]
           [(eq? op '*)
            (make-polynomial +
              (for-each make-polynomial
                        (for-each (lambda (poly)
                                    (differentiate (make-polynomial op poly)))
                        args)
                        (for-each (lambda (poly)
                                    (make-polynomial op (differentiate poly)))
                        args)))]
           [(eq? op '^)
            (make-polynomial +
              (make-polynomial **
                (differentiate (make-polynomial op)))
                (make-polynomial *
                  (list 'constant (current-fraction))
                  (make-polynomial op))))]
           [else (syntax-error "opération inconnue")]))]))

(define (simplify poly)
  (match (syntax->datum poly)
    [(make-polynomial _ (list 'constant s))
     (make-polynomial (current-fraction)
       (list 'constant s))]
    [(make-polynomial _ (list 'variable x))
     (make-polynomial (current-fraction)
       (list 'variable x))]
    [(make-polynomial _ (list 'operation op args ...))
     (cond [(eq? op '+)
            (cond [(for-each zero? args ...)
                    '0]
                  [(eq? (current-fraction) 0)
                   (make-polynomial +
                     (for-each simplify args ...))]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '0))
                              args ...)
                   (make-polynomial +
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [else (make-polynomial +
                            (for-each simplify args ...))])]
           [(eq? op '-')
            (cond [(eq? (current-fraction) 0)
                   (make-polynomial -
                     (for-each simplify args ...))]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '0))
                              args ...)
                   (make-polynomial -
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [else (make-polynomial -
                            (for-each simplify args ...))])]
           [(eq? op '*)
            (cond [(eq? (current-fraction) 0)
                    '0]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '1))
                              args ...)
                   (make-polynomial +
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '0))
                              args ...)
                   '0]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '-1))
                              args ...)
                   (make-polynomial *
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [(for-each (lambda (poly)
                                (eq? (current-fraction)
                                    (syntax->datum poly)))
                              args ...)
                   (make-polynomial +
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [(eq? (current-fraction) 1)
                   (make-polynomial *
                     (for-each simplify args ...))]
                  [else (make-polynomial *
                            (for-each simplify args ...))])]
           [(eq? op '^)
            (cond [(eq? (current-fraction) 0)
                    '1]
                  [(eq? (current-fraction) 1)
                   (make-polynomial +
                     (for-each simplify args ...))]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '0))
                              args ...)
                   '0]
                  [(for-each (lambda (poly)
                                (eq? (syntax->datum poly)
                                    '1))
                              args ...)
                   (make-polynomial +
                     (for-each (lambda (poly)
                                  (simplify poly))
                              args ...))]
                  [(eq? (current-fraction) 2)
                   (make-polynomial **
                     (for-each simplify args ...))]
                  [else (make-polynomial **
                            (for-each simplify args ...))])]
           [else (syntax-error "opération inconnue")]))]))

(define (print-poly poly)
  (let loop ((args (syntax->datum poly)))
    (if (= length args 0)
        '()
        (cond [(eq? (caar args) 'constant)
               (append (cons (write-object (caddar args)) '*)
                       (loop (cddr args)))]
              [(eq? (caar args) 'variable)
               (append (list 'x (list 'exp (caddar args)))
                       (loop (cddr args)))]
              [(eq? (caar args) 'operation)
               (append (list (prin1 (caddar args)) (list 'lparen))
                       (loop (cddr args))
                       (list 'rparen))]))))
```

**Module d'arithmétique à fractions**

```scheme
(define infinity 'inf)
(define undefined 'und)
(define fraction (make-object 'fraction))

(define (simplify-fraction num den)
  (if (zero? num)
      (list '0 '1)
      (let ((gcd (gcd num den)))
        (let ((num (quotient num gcd))
              (den (quotient den gcd)))
          (if (or (negative? num)
                  (negative? den))
              (list 'negate (cons num den))
              (cons num den)))))

(define (*-fraction (lambda (num1 den1 num2 den2)
                      (simplify-fraction
                        (* num1 num2)
                        (* den1 den2))))

(define (+-fraction (lambda (num1 den1 num2 den2)
                      (simplify-fraction
                        (+ (* num1 den2)
                           (* den1 num2))
                        (* den1 den2))))

(define (-fraction (lambda (num1 den1 num2 den2)
                      (simplify-fraction
                        (- (* num1 den2)
                           (* den1 num2))
                        (* den1 den2))))

(define (/fraction (lambda (num1 den1 num2 den2)
                      (simplify-fraction
                        (* num1 den2)
                        (* den1 num2))))
```

**Exemple d'utilisation**

```scheme
(define poly1
  (convert-poly (x^2 - -2x + 1)))

(define result1
  (simplify
    (differentiate poly1)))

(display-object result1)
```

**Explication du code**

**Module de calcul polynomial**

Ce module définit deux fonctions principales : `differentiate` et `simplify`.

* `differentiate` calcule la dérivée d'un polynôme.
* `simplify` simplifie un polynôme en appliquant les règles algébriques de base.

Le code utilise le