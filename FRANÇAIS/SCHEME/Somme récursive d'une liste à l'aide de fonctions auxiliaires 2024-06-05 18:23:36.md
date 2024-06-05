```scheme
;;; Déclaration des fonctions auxiliaires

(définir ma-fonction-aux (n)
  (si (<= n 0) ; Cas de base
    1
    (* n (ma-fonction-aux (- n 1))))) ; Appel récursif

(définir autre-fonction-aux (x y)
  (si (= x 0) ; Condition de sortie
    y
    (autre-fonction-aux (- x 1) (+ y x)))) ; Appel récursif

;;; Déclaration de la fonction principale

(définir ma-fonction-principale (liste)
  (si (cons? liste) ; Cas où la liste n'est pas vide
    (+ (car liste) (ma-fonction-principale (cdr liste))) ; Appel récursif
    0)) ; Cas où la liste est vide

;;; Exemple d'utilisation

(afficher (ma-fonction-principale '(1 2 3 4 5))) ; Affiche 15
```

**Explication du code**

Ce code Scheme implémente une fonction récursive qui calcule la somme des éléments d'une liste. La fonction principale `ma-fonction-principale` appelle récursivement elle-même pour parcourir la liste et additionner les éléments.

La fonction auxiliaire `ma-fonction-aux` calcule le factoriel d'un nombre donné. Elle utilise un cas de base pour le nombre 0 et applique récursivement la fonction pour les nombres positifs.

La fonction auxiliaire `autre-fonction-aux` calcule la somme des nombres d'un intervalle donné, en utilisant un cas de sortie pour l'extrémité inférieure et un appel récursif pour l'extrémité supérieure.