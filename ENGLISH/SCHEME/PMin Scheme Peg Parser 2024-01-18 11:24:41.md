```scheme
(define (tree-map f t)
  (cond
    [(atom? t) (f t)]
    [(list? t) (cons (tree-map f (car t)) (tree-map f (cdr t)))]
    [else (error "tree-map: argument must be a tree")]))

(define (tree->list t) (tree-map (lambda (t) (list t)) t))

(define (list->tree l) (tree-map (lambda (l) (car l)) (cdr l)))

(define (pmin-peg-parser lexer)
  (define (pmin->p l)
    (tree-map (lambda (t) (cons (car t) (tree->list (cdr t))))
              l))
  (define (comp-p l) (list 'seq (map pmin->p l)))
  (define (unbox-p p)
    (cond
      [(atom? p) p]
      [(list? p) (comp-p (cdr p))]
      [else (error "unbox-p: argument must be a pmin rule")]))
  (define (rule-p r) (list r))
  (define (make-p name xs)
    (cons name xs))
  (define (make-r name p)
    (list name (unbox-p p)))
  (define (add-peg augs psyms start end)
    (make-p (make-r start (comp-p psyms))
            (make-r end augs)))
  (define (token p)
    (make-p
     (make-r "token"
             (make-p
              'literal
              (lambda (s) (lexer s))))
     "token"))
  (define (cached p) (list 'cached (unbox-p p)))
  (define (choice l) (make-p 'choice (map unbox-p l)))
  (define (option r) (make-p 'option (unbox-p r)))
  (define (many-p p) (make-p 'many (unbox-p p)))
  (define (lazy-p p) (make-p 'lazy (unbox-p p)))
  (define (new-expr) (make-p 'new-expr ()))
  (define (ref-expr n) (make-p 'ref-expr n))
  (define (seq xs) (comp-p xs))

  lexer)

(define lexer (lambda (s)
  (cond
    [(null? s) 'eof]
    [(= (car s) '(')
     (let loop (l)
       (cond
         [(null? l) 'eof]
         [(= (car l) ')') l]
         [else (loop (cdr l))]))
       (loop s))]
    [else 'word])))

(define (peg-parser g)
  (lambda (s)
    (let rec parse (node s)
      (define (fail) (error "parse: unexpected token"))
      (if (null? s) (error "parse: unexpected eof"))
      (cond
        [(eq? node 'eof) (if (null? s) 'eof (fail))]
        [(eq? (car node) 'seq)
         (let loop (l rest s)
           (define (fail) (error "parse: unexpected token"))
           (if (null? l) (if (null? s) s (fail))
             (let ((t (parse (car l) s)))
               (if t
                 (loop (cdr l) rest t)
                 (fail)))))
           (loop l (cdr node) s))]

        [(eq? (car node) 'choice)
         (let loop (l s)
           (define (fail) (if (null? l) (error "parse: no match")
                             (let ((t (parse (car l) s)))
                               (if t t (fail))))))
           (if (null? l) (fail)
             (let ((t (parse (car l) s)))
               (if t (list (car node) t) (fail)))))]

        [(eq? (car node) 'token)
         (if (= s (car (cdr node))) s (fail))]

        [(eq? (car node) 'cached)
         (let loop ()
           (define (fail) (error "parse: unexpected token"))
           (if (null? s) (fail)
             (let ((t (cache (((cadr node) ())) s)))
               (if (null? t) (fail) t))))
           (loop))]

        [(eq? (car node) 'lazy)
         (let loop (g s)
           (define (fail) (error "parse: unexpected token"))
           (if (null? g) s
             (let ((t (parse (car g) s)))
               (if t (loop (cdr g) t) (fail)))))]
           (loop (cdr node) s))]

        [(eq? (car node) 'option)
         (let loop (g s)
           (define (fail) (fail))
           (if (null? g) s
             (let ((t (parse (car g) s)))
               (if t (loop (cdr g) t) (fail)))))]
           (loop (cdr node) s))]

        [(eq? (car node) 'many)
         (let loop (f g s)
           (define (fail) (fail))
           (let ((s' (parse (car g) s)))
             (if (null? s') (f s)
               (loop f (cdr g) s'))))]
           (loop (lambda (s) s) (cdr node) s))]

        [(eq? (car node) 'new-expr)
         (let loop (g s)
           (define (fail) (error "parse: unexpected token"))
           (if (null? g) (error "parse: unfinished rule")
             (let ((n (((car g) ()))))
               (let ((s' (parse (cdr g) s)))
                 (assoc n s')))))]
           (loop (cdr node) s))]

        [(eq? (car node) 'ref-expr)
         (let loop (g s)
           (define (fail) (error "parse: unexpected token"))
           (if (null? g) (error "parse: unfinished rule")
             (let ((n (((car g) ()))))
               (let ((s' (parse (cdr g) s)))
                 (assoc n s')))))]
           (loop (cdr node) s))]
        [else (fail)]))))
    (parse g s)))

```

This code is a Scheme implementation of a parser generator called PMin. PMin allows you to define a grammar for a language, and then it will generate a parser for that language that can be used to parse input text.

The code is quite complex, so I will only provide a brief overview here. The main function is `peg-parser`, which takes a grammar as input and returns a parser function. The parser function takes a string of input text and returns a parse tree if the input is valid, or an error message otherwise.

The grammar is specified using a series of rules. Each rule has a name, a pattern, and an action. The pattern is a regular expression that matches the input text. The action is a Scheme expression that is executed when the pattern matches.

The `parse` function is a recursive function that walks through the input text and matches it against the rules in the grammar. If a rule matches, the action for that rule is executed. The parse function returns a parse tree if the input is valid, or an error message otherwise.

The `lexer` function is a simple lexer that tokenizes the input text. The tokenizer breaks the input text into a series of tokens, which are then passed to the parser.

The `tree-map` function is a helper function that applies a function to each element of a tree. The `tree->list` and `list->tree` functions convert between trees and lists.

The `comp-p` function combines a series of rules into a single rule. The `unbox-p` function removes the outer parentheses from a rule.

The `rule-p`, `make-p`, and `make-r` functions are helper functions that create rules, productions, and rule names.

The `add-peg` function adds a rule to a grammar. The `token` function creates a rule that matches a single token. The `cached` function creates a rule that caches the results of parsing. The `choice` function creates a rule that matches any of a set of rules. The `option` function creates a rule that matches a rule or nothing. The `many-p` function creates a rule that matches one or more occurrences of a rule. The `lazy-p` function creates a rule that is not parsed until it is needed. The `new-expr` and `ref-expr` functions create rules that match new expressions and references to expressions, respectively.

The `seq` function creates a rule that matches a sequence of rules.

The `fail` function is a helper function that raises an error.