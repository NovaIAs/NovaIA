**Module de manipulation de graphe en FORTH**

```forth
: Noeud nouveau ( -- noeud )
  10 allot                 créer un bloc de données de taille 10
  0 fill                  le remplir de 0
;

: Graphe nouveau ( -- graphe )
  50 allot                créer un bloc de données de taille 50
  0 fill                 le remplir de 0
  0 here @ noeud @ set   le 1er noeud est l'indice du graphe
;

: Noeud voisin ajouter ( noeud voisin -- )
  0 nip ( noeud )
  1+ noeud @ noeud + 1+   calculer l'adresse de la liste des voisins du noeud
  0 herein @ append        ajouter le voisin à la liste
;

: Graphe parcours profondeur ( graphe initial -- )
  0 0 2dup noeud noeud  voisin ajouter   créer une liste de départ vide
  1 loop                  répéter jusqu'à ce que la liste soit vide
  over @ nip dup           dépiler le 1er voisin de la liste
  2dup @ nip dup 0=      si le voisin n'a pas été visité
  if                        alors
    nip dup                dépiler le voisin et l'empiler à nouveau
    2dup noeud ajouter    ajouter le voisin à la liste des noeuds visités
    voisin ajouter        ajouter le voisin à la liste des voisins du noeud d'origine
    voisin ajouter        ajouter le noeud d'origine à la liste des voisins du voisin
    1+                     incrémenter la liste des voisins
    2drop                 dépiler le voisin et la nouvelle taille de liste
    recurse               répéter avec ce noeud
  then                    fin du if
  drop                   dépiler le noeud visité
  2drop                  dépiler la nouvelle taille de liste et l'ancien voisin
;
```

**Utilisation**

```forth
graphe nouveau
10 noeud nouveau
20 noeud nouveau
10 20 noeud ajouter
20 10 noeud ajouter
5 noeud nouveau
10 5 noeud ajouter
20 5 noeud ajouter
graphe parcours profondeur
```

**Explication**

Ce code permet de créer et de manipuler un graphe en FORTH. Le graphe est représenté par un tableau de noeuds, chaque noeud contenant une liste de ses voisins.

La fonction `Noeud nouveau` crée un nouveau noeud et renvoie son adresse.

La fonction `Graphe nouveau` crée un nouveau graphe et renvoie son adresse.

La fonction `Noeud voisin ajouter` ajoute un voisin à la liste de voisins d'un noeud donné.

La fonction `Graphe parcours profondeur` effectue un parcours en profondeur du graphe en partant d'un noeud initial spécifié. Elle visite tous les noeuds accessibles à partir du noeud initial et ajoute chaque noeud visité à une liste de noeuds visités.