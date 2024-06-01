**Code PROLOG complexe en français (explications incluses)**

```prolog
% Représentation des connaissances

% Faits
pere(pierre, jean).
mere(marie, jean).
age(jean, 10).
ville(jean, paris).

% Règles
fils(X, Y) :- pere(Y, X).
fille(X, Y) :- mere(Y, X).
adulte(X) :- age(X, Age), Age > 18.

% Requêtes

% Trouver le père de Jean
pere_de_jean(X) :- pere(X, jean).

% Trouver tous les enfants de Marie
enfants_de_marie(Enfants) :-
    findall(Enfant, mere(marie, Enfant), Enfants).

% Vérifier si Jean est adulte
adulte_jean :-
    age(jean, Age),
    Age > 18.

% Trouver les habitants de Paris âgés de plus de 18 ans
habitants_adultes_de_paris(Habitants) :-
    findall(Habitant, (ville(Habitant, paris), adulte(Habitant)), Habitants).

% Explications

% Faits : Déclarations simples de connaissances.
% Règles : Infèrent de nouvelles connaissances à partir des faits.
% Requêtes : Questions posées au système.

% Les variables commencent par des lettres majuscules.
% Les prédicats se terminent par un point.
% Les arguments sont séparés par des virgules.

% findall/3 : Trouve toutes les solutions à un but donné et les rassemble dans une liste.
% age/2 : Prédicat de fait pour l'âge d'une personne.
% adulte/1 : Prédicat de règle pour vérifier si une personne est adulte.
% ville/2 : Prédicat de fait pour la ville de résidence d'une personne.
```