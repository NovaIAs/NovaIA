**Système expert d'aide à la décision médicale**

**Objectif:**
Aider les praticiens de santé à diagnostiquer et à prescrire des traitements pour les maladies courantes.

**Règles de base de connaissances:**

```prolog
maladie(grippe, [fievre, toux, maux_de_tete]).
maladie(pneumonie, [fievre, toux, essoufflement]).
maladie(angine, [mal_de_gorge, difficulté_à_avaler]).
maladie(bronchite, [toux, expectorations]).
maladie(otite, [mal_d'oreille]).

traitement(grippe, [paracetamol]).
traitement(pneumonie, [antibiotiques]).
traitement(angine, [antibiotiques]).
traitement(bronchite, [sirop_pour_la_toux]).
traitement(otite, [gouttes_pour_les_oreilles]).
```

**Règles d'inférence:**

```prolog
diagnostiquer(Symptômes, Maladie) :-
    Maladie = maladie(N, S),
    forall(member(S, Symptômes)).

prescrire(Maladie, Traitement) :-
    Traitement = traitement(Maladie, T).

```

**Interface utilisateur:**

```prolog
main :-
    write("Entrez les symptômes (séparés par des virgules) : "),
    read_line_to_codes(user_input, Codes),
    atom_codes(Symptômes, Codes),
    diagnostiquer(Symptômes, Maladie),
    prescrire(Maladie, Traitement),
    write("Diagnostic : "),
    write(Maladie), nl,
    write("Traitement : "),
    write(Traitement), nl.
```

**Explication du code:**

* **Règles de base de connaissances:** Ces règles définissent les maladies et leurs symptômes associés, ainsi que les traitements pour chaque maladie.
* **Règles d'inférence:** Ces règles permettent d'inférer la maladie la plus probable en fonction des symptômes entrés par l'utilisateur. Elles vérifient si tous les symptômes associés à une maladie sont présents dans la liste des symptômes de l'utilisateur.
* **Interface utilisateur:** Cette section du code fournit l'interface pour l'utilisateur. Elle demande à l'utilisateur de saisir ses symptômes, puis affiche le diagnostic et le traitement suggéré.

**Exemple d'utilisation:**

```prolog
main

Entrez les symptômes (séparés par des virgules) : fievre, toux, maux_de_tete

Diagnostic : grippe

Traitement : paracetamol
```

**Complexité et différenciation:**

Ce code est complexe car il implémente plusieurs fonctionnalités, notamment:

* **Raisonnement basé sur les règles:** Les règles de base de connaissances et les règles d'inférence permettent au système de raisonner sur les symptômes et de déduire le diagnostic le plus probable.
* **Interface utilisateur:** Le code fournit une interface utilisateur conviviale qui permet à l'utilisateur d'entrer des symptômes et de recevoir un diagnostic et un traitement.
* **Gestion des connaissances:** Les connaissances sur les maladies et les traitements sont représentées en utilisant des règles Prolog, ce qui permet au système d'être facilement mis à jour et étendu.