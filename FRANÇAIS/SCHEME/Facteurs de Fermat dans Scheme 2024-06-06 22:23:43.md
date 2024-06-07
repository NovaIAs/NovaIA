**Code Scheme complexe**

**Objectif :** Déterminer les facteurs d'un entier en utilisant la factorisation de Fermat.

```scheme
(define (fermat-factors n threshold)
  (let loop ((a (sqrt (* n threshold))) (b (- n (* a a))) acc '())
    (if (or (< a threshold) (= b 0))
      acc
      (loop (modulo a b) (square b) (cons a acc)))))
```

**Explication :**

* **fermat-factors** est la fonction principale qui prend deux arguments : **n**, le nombre à factoriser, et **threshold**, le seuil pour arrêter la factorisation.
* La fonction utilise une boucle avec **loop**.
* Dans la boucle, **a** est l'estimation de la racine carrée de **n** multipliée par **threshold**.
* **b** est le reste lorsqu'on divise **n** par **a^2**.
* **acc** est une liste des facteurs trouvés jusqu'à présent.
* La condition de sortie se produit lorsque **a** est inférieur à **threshold** ou que **b** est nul, indiquant que **n** est un nombre premier.
* Si **b** n'est pas nul, la boucle continue avec **a** et **b** mis à jour.
* Si **a** est un facteur de **n**, il est ajouté à la liste **acc**.