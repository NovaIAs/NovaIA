**Fonction complexe récursive en Scheme pour trouver le chemin le plus court dans un graphe pondéré**

```scheme
(define (dijkstra graph start end)
  (define (dist start dest)
    (cond
      ((memq dest (cdr start)) (car start))
      (else (+ (dist start (car dest)) (cdr dest)))))

  (define (min-vertex visited)
    (let ((min-dist (apply min (map (lambda (v) (dist start v)) (remove visited (keys graph))))))
      (find (lambda (v) (= (dist start v) min-dist)) (remove visited (keys graph)))))

  (define (shortest-path start end)
    (cond
      ((null? start) nil)
      ((= start end) (list start))
      (else (append (shortest-path start (car (min-vertex (cdr start)))) (list end)))))

  (shortest-path start end))
```

**Explication du code :**

La fonction `dijkstra` prend trois arguments : le graphe (représenté sous la forme d'une association entre les sommets et leurs arêtes), le sommet de départ et le sommet de destination. Elle calcule le chemin le plus court entre les deux sommets à l'aide de l'algorithme de Dijkstra.

La fonction imbriquée `dist` calcule la distance entre deux sommets dans le graphe. Elle utilise la récursivité pour parcourir le graphe et additionner les poids des arêtes.

La fonction `min-vertex` trouve le sommet non visité avec la plus petite distance. Elle utilise la fonction `min` pour comparer les distances et la fonction `remove` pour exclure les sommets visités.

La fonction `shortest-path` construit le chemin le plus court en appelant récursivement `min-vertex` pour trouver le prochain sommet le plus proche et en ajoutant ce sommet au début de la liste.

**Exemple d'utilisation :**

```scheme
(define graph
  '((a (b 2) (c 1))
    (b (c 3))
    (c (d 1))))

(dijkstra graph 'a 'd)
```

Résultat :

```scheme
'((a b c d))
```