**Code FORTH complexe**

```forth
: contexte-installer ( adresse nom -- )
  2dup @ tableaux-taille tableaux-mettre
  execute
  ;
: contexte-sauver ( -- )
  et tableaux-mettre tableaux-taille @
  ;
: contexte-reprendre ( -- )
  tableaux-taille @ tableaux-prendre execute
  ;

: mot-déplacer ( adresse source adresse destination taille -- )
  temp [ 4 ] allocate do
    i @ source @ + over destination @ + !
    i 1 + !
  loop drop ;

: interp ( adresse -- )
  code clone
  interpreter
  ;

: variable ( adresse -- )
  2dup 1 @
  case
    of 0 :const ( adresse -- ) -- valeur constante
    of 1 :var ( adresse -- ) -- variable
    of 2 :non ( adresse -- ) -- variable non définie
    otherwise halt
  endcase
  ;

: cellule ( adresse -- )
  0 variable = if
    @
  then
  ;

: cellput ( adresse valeur -- )
  0 variable = if
    !
  then
  ;

: compilation ( adresse -- )
  dup swap cellput nip nip variable
  ;

: amorce ( adresse -- )
  0 compilation non interp
  ;

: exécutable ( -- adresse )
  compile
  0 compilation non interp
  ;

: nom-installer ( adresse adresse -- )
  nomdup
  over cellules @
  tableaux-taille @ word dump space
  !
  cellules 1 + @
  4 + neighborhoods-ajouter
  ;

: cellule-nom ( adresse -- adresse )
  cellules @
  while
    Neighborhoods-afficher
    dup 4 + @ = if
      Cellule-afficher pause
      break
    then
    Neighborhoods-suivre
  repeat
  ;

: interpréteur ( -- )
  100000 Cellules-créer
  [ :interpreter ( appareil -- )
      apparail @ over @ interpret ;
  ] Interprète-charger

  100000 Cellules-créer
  [ :compile ( appareil -- )
      apparail @ over @ compile ;
  ] Compilateur-charger

  amorce interpreter
  ;

: word ( -- adresse )
  4 Cellules-créer non
  1 cellules @
  ;

: conseils ( -- )
  100 appareils-nombre
  Appareils-afficher
  100 variables-nombre
  Variables-afficher
  100 tableaux-nombre
  Tableaux-afficher
  100 mots-nombre
  Mots-afficher
  ;

: comptes ( -- )
  Conseils
  Nom-liste
  ;

: mots-créer ( -- )
  mot Consante créer
  mot Constante créer
  mot Proc créer
  mot Compare créer
  mot Dup créer
  mot Échange créer
  mot Gauche créer
  mot Droite créer
  mot Etc créer
  mot If créer
  mot Interprète créer
  mot Lire créer
  mot Nom créer
  mot Non créer
  mot Par créer
  mot Pause créer
  mot Plus créer
  mot Moins créer
  mot Multi créer
  mot Diviser créer
  mot Mod créer
  mot Ne pas créer
  mot Ou créer
  mot Racine créer
  mot Remise créer
  mot Retour créer
  mot Remplacer créer
  mot Saut créer
  mot Same créer
  mot Supprimer créer
  mot Taper créer
  mot Tirets créer
  mot Un plus créer
  mot Un moins créer
  mot Variable créer
  mot Zéro créer
  ;
```

**Explication du code**

Ce code FORTH complexe met en œuvre un interpréteur et un compilateur FORTH, avec des fonctionnalités avancées telles que :

* Création et manipulation de variables, de tableaux et de mots.
* Compilation de mots et interprétation de code.
* Gestion des contextes (pour la création de "mots" imbriqués).
* Gestion des noms (pour l'affichage de noms symboliques au lieu d'adresses).
* Fonctions utilitaires pour la manipulation de cellules et de mots.
* Fonctionnalités de débogage (affichage des comptes de cellules, de mots, etc.).

Le code définit plusieurs mots primitifs FORTH (par exemple, "conste", "var", "interpréter", "compiler"), ainsi que des mots de haut niveau pour la manipulation des données et le contrôle de flux.

L'interpréteur fonctionne en chargeant un mot dans la cellule d'appareil (apparail) et en appelant la fonction "interpreter", qui exécute le code. Le compilateur fonctionne de la même manière, mais il compile le code au lieu de l'interpréter.

Les mots "contexte-installer" et "contexte-reprendre" permettent de gérer les contextes, en sauvegardant et en restaurant l'état de la pile. Le mot "nom-installer" associe des noms symboliques aux adresses.

Le mot "comptes" affiche les comptes de cellules, de mots, etc. Le mot "mots-créer" définit un ensemble de mots primitifs FORTH.

Ce code fournit une base puissante pour la création d'applications FORTH complexes.