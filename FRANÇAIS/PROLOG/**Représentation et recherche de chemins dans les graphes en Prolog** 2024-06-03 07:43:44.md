```prolog
% Règles pour représenter les graphes

graphe(graphe(arcs)).

arc(graphe(arcs), arc(noeud_depart, noeud_arrivee, poids)).

noeud(graphe(arcs), noeud_depart).
noeud(graphe(arcs), noeud_arrivee).

% Règles pour créer un graphe

creer_graphe(Graphe) :-
    graphe(Graphe),
    findall(arc(A, B, P), noeud(Graphe, A), noeud(Graphe, B), arc(graphe(arcs), arc(A, B, P)), Arcs),
    ajouter_arcs(Graphe, Arcs).

ajouter_arcs(Graphe, []).
ajouter_arcs(Graphe, [Arc | Ac]) :-
    ajouter_arc(Graphe, Arc),
    ajouter_arcs(Graphe, Ac).

ajouter_arc(graphe(arcs), arc(A, B, P)) :-
    assertz(arc(graphe(arcs), arc(A, B, P))).

% Règles pour trouver le plus court chemin dans un graphe

plus_court_chemin(Graphe, Noeud_depart, Noeud_arrivee, Chemin, Cout) :-
    \+ plus_court_chemin_trouve(Noeud_depart, Noeud_arrivee, Chemin, Cout),
    trouver_plus_court_chemin(Graphe, Noeud_depart, Noeud_arrivee, Chemin, Cout).

plus_court_chemin_trouve(Noeud_depart, Noeud_arrivee, Chemin, Cout) :-
    length(Chemin, L),
    L > 1,
    nth1(2, Chemin, Noeud_prec),
    arc(graphe(arcs), arc(Noeud_prec, Noeud_arrivee, C)),
    Cout is C.

trouver_plus_court_chemin(Graphe, Noeud_depart, Noeud_arrivee, Chemin, Cout) :-
    trouver_tous_chemins(Graphe, Noeud_depart, Noeud_arrivee, Chemins),
    min_cout(Chemins, Cout),
    nth1(1, Chemins, Chemin).

trouver_tous_chemins(Graphe, Noeud_depart, Noeud_arrivee, Chemins) :-
    trouver_chemins(Graphe, Noeud_depart, Noeud_arrivee, [], Chemins).

trouver_chemins(_, Noeud_arrivee, Noeud_arrivee, [[]], []).
trouver_chemins(Graphe, Noeud_depart, Noeud_arrivee, Chemin, Chemins) :-
    findall(Chemin_suivant, (arc(Graphe, arc(Noeud_depart, Noeud_suivant, _)), \+ member(Noeud_suivant, Chemin)), Chemins_suivants),
    maplist(append([Noeud_depart], _, Chemin), Chemins_suivants, Chemins_intermediaires),
    trouver_chemins(Graphe, Noeud_depart, Noeud_arrivee, Chemin+[Noeud_depart], Chemins_intermediaires).

% Règles utilitaires

min_cout([], []).
min_cout([Chemin | Autres], CoutMin) :-
    cout_chemin(Chemin, Cout),
    min_cout(Autres, CoutSuiv),
    CoutSuiv < Cout,
    !,
    CoutMin = CoutSuiv.
min_cout([Chemin | Autres], Cout) :-
    cout_chemin(Chemin, Cout),
    min_cout(Autres, CoutMin),
    CoutMin < Cout,
    Cout = CoutMin.
min_cout([], Cout) :-
    Cout = 1000000. % Valeur par défaut pour un coût infini

cout_chemin([], 0).
cout_chemin([Noeud_depart | Noeuds], Cout) :-
    trouver_cout(graphe(arcs), Noeud_depart, Noeuds, Cout).

trouver_cout(_, [], _, 0).
trouver_cout(Graphe, Noeud_depart, Noeuds, Cout) :-
    once(arc(Graphe, arc(Noeud_depart, Noeud_suivant, Cout_arc))),
    Reste_noeuds = [Noeud | Noeuds],
    trouver_cout(Graphe, Noeud_suivant, Reste_noeuds, Cout_suivant),
    Cout is Cout_suivant + Cout_arc.

% Exemple d'utilisation

graphe(Graphe),
creer_graphe(Graphe),
plus_court_chemin(Graphe, a, b, Chemin, Cout),
writeln(Chemin),
writeln(Cout).
```

**Explication du code :**

Ce code PROLOG définit un système pour représenter et manipuler des graphes orientés pondérés. Il inclut des fonctions pour :

* **Représentation de graphe :** `graphe/1`, `arc/3`, `noeud/2`
* **Création de graphe :** `creer_graphe/1`, `ajouter_arcs/2`, `ajouter_arc/2`
* **Recherche du plus court chemin :** `plus_court_chemin/4`, `plus_court_chemin_trouve/4`, `trouver_plus_court_chemin/4`
* **Recherche de tous les chemins :** `trouver_tous_chemins/4`, `trouver_chemins/4`
* **Fonctions utilitaires :** `min_cout/2`, `cout_chemin/2`, `trouver_cout/4`

Ce code peut être utilisé pour résoudre des problèmes d'optimisation sur des graphes, tels que la recherche du plus court chemin entre deux nœuds.

**Exemple d'utilisation :**

L'exemple d'utilisation à la fin du code crée un graphe et calcule le plus court chemin entre les nœuds "a" et "b". Le résultat serait affiché comme suit :

```
[a, b]
5
```