**Définition du lexique**

```forth
: liste_creer ( nb ) -- liste vide
  dup 0 do
    0 >r
  loop drop ;
```

**Fonctions d'accès à la liste**

```forth
: liste_taille ( liste ) -- nombre d'éléments
  nip 0 do i+ loop ;

: liste_element ( liste index ) -- élément indexe
  dup 0 do i+ swap c@ loop drop ;

: liste_modifier ( liste index valeur ) --
  dup 0 do i+ swap c! loop drop ;
```

**Fonctions d'ajout à la liste**

```forth
: liste_debut_ajouter ( liste valeur ) --
  0 swap 2* + c@ dup 1+ c! swap c! ;

: liste_fin_ajouter ( liste valeur ) --
  dup liste_taille swap 1+ * c@ c! swap 1+ c! ;
```

**Fonctions de suppression de la liste**

```forth
: liste_debut_supprimer ( liste ) --
  dup 0 do
    i+ swap c@ 1- c! i+ c@ swap c!
  loop drop ;

: liste_fin_supprimer ( liste ) --
  dup liste_taille 1- do
    i+ swap c@ 1- c! i+ c@ swap c!
  loop drop ;
```

**Fonctions d'itération de liste**

```forth
: liste_pour ( liste ) --
  dup 0 1+ swap
  begin
    over liste_taille 1+ > while
      swap liste_element
      over
    then ;
```

**Exemple d'utilisation**

```forth
20 liste_creer variable ma_liste
15 liste_debut_ajouter ma_liste
27 liste_fin_ajouter ma_liste
13 liste_fin_ajouter ma_liste

ma_liste liste_pour do
  cr .
loop
```

**Explication du code**

* Le lexique `liste_creer` crée une liste vide de la taille spécifiée.
* Les fonctions `liste_taille`, `liste_element` et `liste_modifier` permettent respectivement d'accéder, de modifier et de récupérer des éléments de la liste.
* Les fonctions `liste_debut_ajouter` et `liste_fin_ajouter` ajoutent des éléments au début et à la fin de la liste.
* Les fonctions `liste_debut_supprimer` et `liste_fin_supprimer` suppriment des éléments du début et de la fin de la liste.
* La fonction `liste_pour` itère sur la liste et exécute un bloc de code pour chaque élément.
* L'exemple de code crée une liste, lui ajoute quelques éléments, puis l'affiche à l'aide de la fonction `liste_pour`.