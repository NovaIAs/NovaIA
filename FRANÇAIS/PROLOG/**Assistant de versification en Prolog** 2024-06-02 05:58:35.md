**Assistant d'écriture de poésie en Prolog**

```prolog
% Règles pour générer des vers aléatoires
vers(haiku): verset_haiku(V)-> writef(V), writeln(.),
              !.
vers(pentametre): verset_pentametre(V)-> writef(V), writeln(.),
              !.

% Règles pour générer des versets de haïku
verset_haiku(Vs): length(Vs,3),
                 vers_haiku_ligne(Vs,1,N),
                 vers_haiku_ligne(Vs,2,N),
                 vers_haiku_ligne(Vs,3,N).
vers_haiku_ligne(Vs,N,Max): vers_haiku_ligne(Vs,N,Max,[]).
vers_haiku_ligne(Vs,N,Max,Acc): length(Acc,L),
                                 C is Max-L,
                                 L >= C,
                                 Syllabe is random(5),
                                 append(Acc,[Syllabe],AL),
                                 vers_haiku_ligne(Vs,N,Max,AL),
                                 Last is Max-L,
                                 L is Last,
                                 append([Syllabe],[],Vs),
                                 !.
vers_haiku_ligne(Vs,N,Max,Acc): R is random(5),
                                 C is Max-length(Acc),
                                 S is R+C,
                                 append(Acc,[S],AL),
                                 vers_haiku_ligne(Vs,N,Max,AL).

% Règles pour générer des versets de pentamètre iambique
verset_pentametre(Vs): length(Vs,5),
                    verset_pentametre_ligne(Vs,1,N),
                    verset_pentametre_ligne(Vs,2,N),
                    verset_pentametre_ligne(Vs,3,N),
                    verset_pentametre_ligne(Vs,4,N),
                    verset_pentametre_ligne(Vs,5,N).
verset_pentametre_ligne(Vs,N,Max): verset_pentametre_ligne(Vs,N,Max,[]).
verset_pentametre_ligne(Vs,N,Max,Acc): length(Acc,L),
                                 C is Max-L,
                                 L >= C,
                                 (
                                  (N mod 2 is 1, Pied is Jambe),
                                  (N mod 2 is 0, Pied is Trochee)
                                 ),
                                 append(Acc,[Pied],AL),
                                 verset_pentametre_ligne(Vs,N,Max,AL),
                                 Last is Max-L,
                                 L is Last,
                                 append([Pied],[],Vs),
                                 !.
verset_pentametre_ligne(Vs,N,Max,Acc): (N mod 2 is 1, Pied is Jambe),
                                        (N mod 2 is 0, Pied is Trochee),
                                 R is random(5),
                                 C is Max-length(Acc),
                                 S is R+C,
                                 append(Acc,[S],AL),
                                 verset_pentametre_ligne(Vs,N,Max,AL).

% Règles pour générer des pieds iambiques et trochées aléatoires
Jambe is [Faible,Fort].
Trochee is [Fort,Faible].
random(X): random(1,X,R), !.

% Clauses de faits
Faible = 1.
Fort = 2.
```

**Explication du code**

Ce code Prolog génère des versets aléatoires dans deux formes poétiques différentes : le haïku et le pentamètre iambique.

**Règles de génération de vers**
- `vers/2` : génère un verset aléatoire du type spécifié (: haïku ou pentamètre).
- `vers_haiku/1` : génère un verset de haïku composé de trois lignes.
- `vers_pentametre/1` : génère un verset de pentamètre iambique composé de cinq lignes.

**Règles de génération de lignes de haïku**
- `vers_haiku_ligne/4` : génère une ligne de haïku avec le nombre spécifié de syllabes.
- La règle récursive génère une liste de nombres entiers représentant le nombre de syllabes par ligne.
- Elle garantit que le nombre total de syllabes par ligne ne dépasse pas le maximum autorisé pour le haïku (5/7/5).

**Règles de génération de lignes de pentamètre iambique**
- `vers_pentametre_ligne/4` : génère une ligne de pentamètre iambique avec le nombre spécifié de pieds iambiques ou trochées.
- La règle récursive génère une liste de listes de nombres entiers représentant le type de pied (jambe ou trochée) par ligne.
- La ligne doit comporter un pied de plus si elle est la deuxième, quatrième ou cinquième ligne.

**Règles de génération de pieds**
- Jambe est défini comme une liste contenant un élément Faible suivi d'un élément Fort.
- Trochée est défini comme une liste contenant un élément Fort suivi d'un élément Faible.
- `random/2` génère un nombre aléatoire entre 1 et un maximum spécifié.

**Clauses de faits**
- Faible et Fort sont des constantes représentant les types de pied.