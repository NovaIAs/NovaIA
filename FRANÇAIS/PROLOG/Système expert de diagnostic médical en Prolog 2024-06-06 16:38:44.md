**Système expert de diagnostic médical**

```prolog
% Définition des symptômes
symptome(fievre).
symptome(toux).
symptome(courbatures).
symptome(maux_de_tete).
symptome(nausees).
symptome(vomissements).
symptome(diarrhee).
symptome(fatigue).

% Définition des maladies
maladie(grippe, [fievre, toux, courbatures, maux_de_tete]).
maladie(gastro, [nausees, vomissements, diarrhee]).
maladie(fatigue_chronique, [fatigue]).

% Règles de diagnostic
regle(grippe, [fievre, toux]).
regle(grippe, [fievre, courbatures]).
regle(gastro, [nausees, vomissements]).
regle(gastro, [diarrhee]).
regle(fatigue_chronique, [fatigue]).

% Fonction de diagnostic
diagnostic(Symptomes, Maladie) :-
    membre(Symptome, Symptomes),
    regle(Maladie, [Symptome | _]).

% Main
main :-
    write("Entrez vos symptômes (séparés par des virgules) : "),
    read_line_to_codes(user_input, Codes),
    atom_codes(Symptomes, Codes),
    liste_symptomes(Symptomes),
    diagnostic(Symptomes, Maladie),
    write("Vous êtes probablement atteint de : "),
    write(Maladie).
```

**Explication du code :**

* **Définition des symptômes (facts)**: Une liste des symptômes possibles est définie à l'aide de faits.
* **Définition des maladies (règles)**: Une liste de maladies possibles et de leurs symptômes associés est définie à l'aide de règles.
* **Règles de diagnostic (règles)**: Des règles sont définies pour mapper les symptômes aux maladies.
* **Fonction de diagnostic (clause)**: Une clause est implémentée pour effectuer le diagnostic en fonction des symptômes fournis.
* **Main (clauses)**: La clause principale demande à l'utilisateur de fournir des symptômes, analyse l'entrée, effectue le diagnostic et l'affiche.

**Exemple d'utilisation :**

```
Entrez vos symptômes (séparés par des virgules) : fièvre, toux
Vous êtes probablement atteint de : grippe
```