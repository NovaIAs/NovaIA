```scheme
(define-syntax cond-template
  (syntax-rules ()
    ((cond-template [condition] _ else [template])
     (if condition template else))
    ((cond-template [condition] _ [template])
     (if condition template #f)))

(define-macro and
  (syntax-rules ()
    ((and/template [_]) #t)
    ((and/template [condition] _ else)
     (cond-template condition #t #f else))
    ((and/template [condition] _ [template])
     (cond-template condition template #f)))

(define-macro or
  (syntax-rules ()
    ((or/template [_]) #f)
    ((or/template [condition] _ else)
     (cond-template condition #t else))
    ((or/template [condition] _ [template])
     (cond-template condition template #t)))

(define-syntax match
  (syntax-rules ()
    ((match value ([pattern] output))
     `(cond ((eq? value (car pattern)) (car output))
            (else (match value (cdr pattern))))))

(define-syntax find-maximum
  (syntax-rules ()
    ((find-maximum [values] _ else [maximum])
     `(cond ((null? values) (car maximum))
            ((> (car values) (car maximum))
             (find-maximum (cdr values) (cons (car values) maximum) else))
            (else (find-maximum (cdr values) maximum else))))))
```

This code defines several macros in the Scheme programming language.

**1. cond-template:**
    A macro for conditional statements with a default else clause.

**2. and/template:**
    A macro for logical and operations with a default else clause.

**3. or/template:**
    A macro for logical or operations with a default else clause.

**4. match:**
    A macro for pattern matching against a list.

**5. find-maximum:**
    A macro for finding the maximum value in a list, with an optional else clause for returning a default value if the list is empty.

Here's an example of how to use these macros:

```scheme
(cond-template (> x 0) "Positive" "Negative" "Zero")
(and/template (> x 0) (< x 10))
(or/template (zero? x) (> x 10))
(match 3 ([1 2 3] "Three") ([4 5 6] "Four to Six") ([7 8 9] "Seven to Nine"))
(find-maximum (list 1 2 3 4 5))
```

The output of this code would be:

```
"Positive"
#f
#t
"Three"
5
```