**Définition des fonctions de manipulation de listes**

```scheme
(define (cons x y)
  (pair x y))

(define (car p)
  (first p))

(define (cdr p)
  (rest p))

(define (append l1 l2)
  (cond ((empty? l1) l2)
        ((empty? l2) l1)
        (else (cons (car l1) (append (cdr l1) l2)))))

(define (reverse l)
  (cond ((empty? l) '())
        (else (append (reverse (cdr l)) (list (car l))))))

**Définition de la structure de données de l'arbre binaire**

```scheme
(define-struct arbre (gauche droite valeur))

**Fonctions de manipulation d'arbres binaires**

(define (arbre-vide)
  (make-arbre '() '() '()))

(define (feuille v)
  (make-arbre '() '() v))

(define (arbre g d v)
  (make-arbre g d v))

(define (gauche a)
  (arbre-gauche a))

(define (droite a)
  (arbre-droite a))

(define (valeur a)
  (arbre-valeur a))

(define (vide? a)
  (= a '()))

(define (feuille? a)
  (and (vide? (gauche a))
       (vide? (droite a))))

**Fonctions de parcours d'arbres binaires**

(define (parcours-prefixe a)
  (cond ((vide? a) '())
        ((feuille? a) (list (valeur a)))
        (else (cons (valeur a)
                    (append (parcours-prefixe (gauche a))
                            (parcours-prefixe (droite a)))))))

(define (parcours-infixe a)
  (cond ((vide? a) '())
        ((feuille? a) (list (valeur a)))
        (else (append (parcours-infixe (gauche a))
                       (list (valeur a))
                       (parcours-infixe (droite a))))))

(define (parcours-postfixe a)
  (cond ((vide? a) '())
        ((feuille? a) (list (valeur a)))
        (else (append (parcours-postfixe (gauche a))
                       (parcours-postfixe (droite a))
                       (list (valeur a))))))

**Fonction principale de test**

(define a (arbre (arbre (feuille 1) (feuille 2) 3)
                  (feuille 4)
                  5))

(display (parcours-prefixe a))
(newline)
(display (parcours-infixe a))
(newline)
(display (parcours-postfixe a))

**Explication du code**

* Les fonctions de manipulation de listes définies au début du code sont des implémentations standard de `cons`, `car`, `cdr`, `append` et `reverse`.
* La structure de données de l'arbre binaire est définie à l'aide du formulaire `define-struct`. Chaque arbre est représenté par une paire `(gauche droite)` d'arbres et une valeur `v`.
* Les fonctions de manipulation d'arbres binaires, telles que `arbre-vide`, `feuille` et `arbre`, créent et manipulent des arbres binaires.
* Les fonctions de parcours d'arbres binaires, `parcours-prefixe`, `parcours-infixe` et `parcours-postfixe`, parcourent un arbre binaire dans les ordres préfixe, infixe et postfixe, respectivement.
* La fonction principale de test crée un arbre binaire et affiche son parcours dans les trois ordres.

Ce code fournit une implémentation complète des structures de données et des fonctions nécessaires pour créer et parcourir des arbres binaires en Scheme.