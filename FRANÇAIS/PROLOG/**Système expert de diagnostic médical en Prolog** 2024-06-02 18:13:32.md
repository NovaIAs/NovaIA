**Système expert de diagnostic médical**

```prolog
:- dynamic maladie/2, symptome/2, regle/3.

% Base de connaissances

maladie(grippe, "Grippe").
maladie(bronchite, "Bronchite").
maladie(pneumonie, "Pneumonie").

symptome(fievre, "Fièvre").
symptome(toux, "Toux").
symptome(essoufflement, "Essoufflement").

% Règles

% Si le patient a de la fièvre et tousse, alors il a la grippe.
regle(maladie(grippe), [symptome(fievre), symptome(toux)]).

% Si le patient a de la toux et de l'essoufflement, alors il a la bronchite.
regle(maladie(bronchite), [symptome(toux), symptome(essoufflement)]).

% Si le patient a de la fièvre, de la toux et de l'essoufflement, alors il a la pneumonie.
regle(maladie(pneumonie), [symptome(fievre), symptome(toux), symptome(essoufflement)]).

% Fonctions de diagnostic

% Demande des symptômes au patient.
demande_symptomes(ListeSymptomes) :-
    write("Veuillez entrer la liste de vos symptômes (séparés par des virgules) : "),
    read_line_to_codes(user_input, Codes),
    atom_codes(ListeSymptomesCodes, Codes),
    string_to_list(ListeSymptomesCodes, ListeSymptomes).

% Convertit une liste de symptômes en une liste de prédicats.
symptomes_vers_faits(ListeSymptomes, ListeFaits) :-
    maplist(symptome_vers_fait, ListeSymptomes, ListeFaits).

symptome_vers_fait(Symptome, fait(symptome(Symptome))) :-
    symptome(Symptome, _).

% Applique les règles pour diagnostiquer la maladie.
diagnostiquer(ListeFaits, Maladie) :-
    findall(Maladie, (regle(Maladie, Conditions), membre(Fait, ListeFaits), memberchk(Fait, Conditions)), Maladies),
    select(Maladie, Maladies, _).

% Interface utilisateur

menu :-
    write("Bienvenue dans le système expert de diagnostic médical."),
    write("Voulez-vous diagnostiquer une maladie ? (oui/non) "),
    read(Reponse),
    (Reponse = oui -> diagnostiquer ; Reponse = non -> halt).

diagnostiquer :-
    demande_symptomes(ListeSymptomes),
    symptomes_vers_faits(ListeSymptomes, ListeFaits),
    diagnostiquer(ListeFaits, Maladie),
    write("Vous avez probablement la "), write(Maladie), write("."),
    write("Consultez un médecin pour confirmer le diagnostic.").
```

**Explication**

Ce code PROLOG est un système expert qui diagnostique les maladies médicales en fonction de la liste des symptômes fournis par le patient. Il comprend une base de connaissances de maladies, de symptômes et de règles, ainsi que des fonctions pour demander les symptômes au patient, convertir la liste des symptômes en faits et appliquer les règles pour diagnostiquer la maladie.

L'utilisateur interagit avec le système via l'interface utilisateur qui affiche un menu d'options. L'utilisateur peut choisir de diagnostiquer une maladie ou de quitter le système. S'il choisit de diagnostiquer une maladie, le système lui demandera la liste de ses symptômes, convertira cette liste en faits et appliquera les règles pour diagnostiquer la maladie. Le système affichera ensuite le diagnostic et recommandera à l'utilisateur de consulter un médecin pour confirmation.