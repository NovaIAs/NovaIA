**Programme FORTH complexe**

```forth
: main
  \ Initialise un tableau de 10 cellules, toutes à 0
  10 0 allot ARRAY

  \ Remplit le tableau avec les valeurs de 1 à 10
  0 1 do
    i ARRAY + i C@
  loop

  \ Affiche le tableau
  "Tableau :" emit cr
  0 10 do
    i ARRAY + i C@ . " "
  loop cr

  \ Trie le tableau par ordre croissant
  bubbleSort ARRAY

  \ Affiche le tableau trié
  "Tableau trié :" emit cr
  0 10 do
    i ARRAY + i C@ . " "
  loop cr
;

\ Fonction de tri par bulles
: bubbleSort ( a -- )
  \ Boucle sur le tableau
  begin
    0 1 do
      \ Boucle interne pour comparer les éléments adjacents
      1 - do
        i INDEX + i ARRAY + 1+ ARRAY C@ > if
          i INDEX + i ARRAY + C@ i INDEX + i ARRAY + 1+ ARRAY C!
          i INDEX + i ARRAY + 1+ ARRAY C@ i INDEX + i ARRAY + C!
        then
      loop
    loop
  again
;
```

**Explications**

* **: main** est le point d'entrée du programme.
* **10 0 allot ARRAY** crée un tableau de 10 cellules initialisées à 0.
* **0 1 do ... loop** est une boucle qui s'exécute 10 fois, de 0 à 9.
* **i ARRAY + i C@** accède à la cellule d'indice `i` dans le tableau `ARRAY`.
* **bubbleSort ARRAY** appelle la fonction de tri par bulles pour trier le tableau `ARRAY`.
* **0 10 do ... loop** est une boucle qui s'exécute 10 fois, de 0 à 9.
* **i INDEX + i ARRAY + C@** accède à la cellule d'indice `i` dans le tableau trié `ARRAY`.
* **i INDEX + i ARRAY + 1+ ARRAY C@ > if ... then** compare la cellule actuelle avec la cellule suivante et effectue un échange si nécessaire.