```forth
: complex-code
  dup 2 + 5 * 3 - 7 / ; 42

: more-complex-code
  2dup 4 + 3 * 5 / 6 + 2dup - ; 21
```

**Explication du code**

: complex-code
  duplique la pile supérieure
  ajoute 2
  multiplie par 5
  soustrait 3
  divise par 7
  renvoie 42

: more-complex-code
  duplique la pile supérieure
  2 fois
  ajoute 4
  multiplie par 3
  divise par 5
  ajoute 6
  duplique la pile supérieure 2 fois
  soustrait
  renvoie 21