**Code FORTH complexe en français**

```forth
: CONCATÉNER-CHAÎNES ( a-chaîne b-chaîne -- chaîne-concaténée )
  SWAP 2SWAP
  :boucle
    C@ 0= IF DROP EXIT THEN
    C@ OVER CAPPEND
    1+ SWAP NEXT
  ;boucle
  ;

: FACTORIELLE ( entier -- factorielle )
  1 IF DUP 1- FACTORIELLE * THEN ;

: PUISSANCE ( base exposant -- puiss )
  1 IF BASE DUP 1- PUISSANCE * THEN ;

: FIBONACCI ( entier -- nombre de Fibonacci )
  < IF 1 THEN
  0= IF FIBONACCI FIBONACCI + THEN ;

: MAXIMUM ( n1 n2 -- maximum )
  IF THEN ;

: MOYENNE ( n1 n2 -- moyenne )
  + 2 / ;

: PREMIER ( entier -- booléen )
  2 < IF
    2 DUP /MOD 0= IF FALSE THEN
    SQRT DUP < IF
      :boucle
        DUP 1+ OVER * -2 DUP /MOD 0= IF FALSE THEN
        1+ SWAP NEXT
      ;boucle
    THEN
  THEN ;

: TRIER-TABLEAU ( tableau -- tableau-trié )
  CREATE :trier
    0 SWAP > IF SWAP ROLL ROT SWAP TRIER THEN ;
  , n > DO I trier LOOP ;

: AFFICHER-TABLEAU ( tableau -- )
  0 SWAP > WHILE I ."  " LOOP ;

: OU ( a b -- a-ou-b )
  IF THEN ;

: ET ( a b -- a-et-b )
  IF
    IF THEN THEN ;

: EXCLUSIF-OU ( a b -- a-exclusif-ou-b )
  IF XOR THEN ;

: DÉCALER-À-GAUCHE ( a b -- a-décalé-à-gauche )
  LSHIFT ;

: DÉCALER-À-DROITE ( a b -- a-décalé-à-droite )
  RSHIFT ;

: INVERSEMENT-DE-BITS ( a -- a-inversé )
  NOT ;

: ROTATION-À-DROITE ( a n -- a-roté-à-droite )
  RROT ;

: ROTATION-À-GAUCHE ( a n -- a-roté-à-gauche )
  LROT ;

: APPELER-CODE-EXTERNE ( adresse -- )
  EXEC ;

: DÉFINIR-FONCTION-EXTERNE ( nom adresse -- )
  HERE INTERPRETER ;

: INTERPRÉTER ( code -- )
  PARSE INTERPRET ;

: COMPILER ( code -- )
  PARSE COMPILE ;
```

**Explication du code**

Ce code FORTH implémente un ensemble de fonctions et de primitives complexes, notamment :

* **CONCATÉNER-CHAÎNES** : concatène deux chaînes de caractères.
* **FACTORIELLE** : calcule la factorielle d'un entier.
* **PUISSANCE** : calcule la puissance d'une base donnée un exposant.
* **FIBONACCI** : calcule le nombre de Fibonacci d'un entier.
* **MAXIMUM** : retourne le maximum de deux nombres.
* **MOYENNE** : calcule la moyenne de deux nombres.
* **PREMIER** : détermine si un entier est premier.
* **TRIER-TABLEAU** : trie un tableau d'entiers en ordre croissant.
* **AFFICHER-TABLEAU** : affiche un tableau d'entiers.
* **OU**, **ET**, **EXCLUSIF-OU** : opérateurs logiques bit à bit.
* **DÉCALER-À-GAUCHE**, **DÉCALER-À-DROITE** : décalent un nombre de bits à gauche ou à droite.
* **INVERSEMENT-DE-BITS** : inverse les bits d'un nombre.
* **ROTATION-À-DROITE**, **ROTATION-À-GAUCHE** : font pivoter les bits d'un nombre vers la droite ou la gauche.
* **APPELER-CODE-EXTERNE** : appelle une fonction externe à partir d'une adresse de mémoire.
* **DÉFINIR-FONCTION-EXTERNE** : définit une fonction externe liée à une adresse de mémoire.
* **INTERPRÉTER**, **COMPILER** : interprètent ou compilent du code FORTH.

Ce code démontre la puissance et la polyvalence du langage FORTH en permettant de définir et d'utiliser des fonctions complexes et des structures de contrôle avancées.