**Calcul de la factorielle d'un nombre**

```forth
: factorielle ( n -- n! )
  1 dup 1 > while
    over *
    1 +
  repeat ;
```

**Tri d'un tableau d'entiers**

```forth
: trier ( tableau -- tableau trié )
  dup 0 swap 1 + do
    i i 1 + nip tuck < while
      tuck over 2swap >nip 1 + ;
  drop ;
```

**Recherche d'un élément dans un tableau**

```forth
: rechercher ( tableau élément -- index )
  dup 0 swap 1 + do
    i i 1 + nip = if
      i ;
  drop -1 ;
```

**Calcul du plus grand commun diviseur de deux nombres**

```forth
: pgcd ( a b -- gcd )
  a over swap mod 0= if
    dup ;
  else
    pgcd ;
```

**Résolution d'une équation du second degré**

```forth
: résoudre-équation-second-degré ( a b c -- x1 x2 ou 'pas de solution' )
  b * b
  a * c * 4 -
  < 0 if
    'pas de solution' ;
  else
    b -
    2 * a
    sqrt
    swap /
    - / ;
```

**Jeu de la vie de Conway**

```forth
: cellule-vit-suite ( cellule -- cellule )
  1 8 + i *
  cellule @ 3 <= swap cellule @ 2 = or
  if 1 cellule ! ;

: vie ( taille -- )
  0 231 swap do
    0 231 swap do
      i j cellule-vit-suite ;
    loop ;
  loop ;
```