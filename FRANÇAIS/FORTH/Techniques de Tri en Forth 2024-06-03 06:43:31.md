```forth
: Trouver-Maximum ( -- n )   \ Trouver le maximum d'une série de nombres
  0 swap 1+ do
    0 > if
      swap drop
    else
      swap > if
        swap
      then
    then
  loop drop ;

: Trier-Par-Insertion ( -- ) \ Trier une série de nombres par insertion
  0 ' Trier-Par-Insertion-Interne ;
  1+ do
    i Trier-Par-Insertion-Interne
  loop drop ;

: Trier-Par-Insertion-Interne ( n -- ) \ Trier une série de nombres par insertion (interne)
  dup i 1+ do
    j < if
      swap j >r swap > if
        j 1+ 1+ swap
      then
    then
  loop drop ;

: Fusionner ( a b -- ) \ Fusionner deux séries de nombres triés
  2 swap do
    j < if
      swap > if
        swap
      then
    then
    1+ j 1+
  loop drop ;

: Trier-Par-Fusion ( -- ) \ Trier une série de nombres par fusion
  0 ' Trier-Par-Fusion-Interne ;
  4 1+ do
    4 * do
      i Trier-Par-Fusion-Interne
    loop
    i Fusionner
  loop drop ;

: Trier-Par-Fusion-Interne ( n -- ) \ Trier une série de nombres par fusion (interne)
  2 dup < if
    swap 0 0
  else
    1+ do
      swap @ > if
        j @ 0
      else
        j @ 1
      then 0
    loop
    swap 0 1
  then ;

: Recherche-Binaire ( n -- i ) \ Rechercher un nombre dans une série de nombres triés par recherche binaire
  0 ' Recherche-Binaire-Interne ;
  1+ do
    i Recherche-Binaire-Interne
  loop drop ;

: Recherche-Binaire-Interne ( n -- i ) \ Rechercher un nombre dans une série de nombres triés par recherche binaire (interne)
  dup i 1+ do
    j < if
      swap j > if
        swap
      then
    then
    1+ j 1+
  loop drop ;
```