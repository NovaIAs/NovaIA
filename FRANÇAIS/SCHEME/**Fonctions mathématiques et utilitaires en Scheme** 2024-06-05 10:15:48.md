```scheme

;; Une fonction récursive qui calcule la factorielle d'un nombre entier
;; non négatif.

(define (factorielle n)
  (if (= n 0) ;; Si n est égal à 0
    1 ;; La factorielle de 0 est 1
    (* n (factorielle (- n 1))))) ;; Sinon, on multiplie n par la 
;; factorielle de n-1

;; Une fonction qui renvoie les nombres premiers jusqu'à une limite
;; donnée.

(define (nombres-premiers n)
  (define (est-premier? x) ;; Une fonction privée qui vérifie si un nombre 
;; est premier
    (for/list ([i (in-range 2 (round (/ x 2)))]),
      (not (= (modulo x i) 0)))) ;; On vérifie si le nombre est divisible 
;; par un nombre entre 2 et sa moitié
  (filter est-premier? (in-range 2 n))) ;; On filtre les nombres entre 2 
;; et n pour ne garder que les nombres premiers

;; Une fonction qui retourne la somme des cubes de tous les nombres 
;; impairs jusqu'à une limite donnée.

(define (somme-cubes-impairs n)
  (for/sum ([i (in-range 1 n 2)]) ;; On itère sur les nombres impairs de 1 
;; à n
    (cube i))) ;; Et on somme leurs cubes

;; Une fonction qui retourne une liste de toutes les permutations d'une liste 
;; donnée.

(define (permutations xs)
  (if (empty? xs) ;; Si la liste est vide
    '(()) ;; On retourne une liste vide
    (for*/list ([x xs]) ;; Sinon, on itère sur les éléments de la liste
      (append (map (lambda (ys) (cons x ys)) (permutations (delete-first x 
;; xs))) ;; Pour chaque élément, on calcule la liste des permutations 
;; des autres éléments et on préfixe chaque permutation par l'élément 
;; courant
      (permutations (delete-first x xs)))))

;; Une fonction qui retourne le produit de tous les éléments d'une liste.

(define (produit xs)
  (if (empty? xs) ;; Si la liste est vide
    1 ;; On retourne 1
    (* (car xs) (produit (cdr xs))))) ;; Sinon, on multiplie le premier 
;; élément par le produit des autres éléments

;; Une fonction qui retourne une chaîne de caractères représentant un nombre 
;; binaire donné.

(define (nombre-binaire n)
  (if (zero? n) ;; Si le nombre est nul
    "0" ;; On retourne "0"
    (string-append (nombre-binaire (quotient n 2)) ;; Sinon, on calcule le 
;; nombre binaire de la partie entière du nombre divisé par 2
                   (string (modulo n 2)))) ;; Et on concatène le reste de la 
;; division par 2, converti en chaîne de caractères

```

**Explication du code :**

Ce code implémente plusieurs fonctions utiles en Scheme, notamment :

* **factorielle:** Calcule la factorielle d'un nombre entier non négatif.
* **nombres-premiers:** Génère une liste de nombres premiers jusqu'à une limite donnée.
* **somme-cubes-impairs:** Calcule la somme des cubes de tous les nombres impairs jusqu'à une limite donnée.
* **permutations:** Génère une liste de toutes les permutations d'une liste donnée.
* **produit:** Calcule le produit de tous les éléments d'une liste.
* **nombre-binaire:** Convertit un nombre décimal en une chaîne de caractères représentant sa représentation binaire.

Chaque fonction est implémentée de manière récursive ou itérative, et utilise des fonctions auxiliaires privées lorsque cela est nécessaire. Les fonctions sont commentées pour faciliter leur compréhension.