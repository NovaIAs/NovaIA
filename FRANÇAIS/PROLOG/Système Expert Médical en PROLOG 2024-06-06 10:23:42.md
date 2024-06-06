**Système Expert Médical en PROLOG**

```prolog
% Définir les symptômes
symptome(fievre).
symptome(toux).
symptome(mal_de_gorge).
symptome(courbatures).
symptome(fatigue).

% Définir les maladies
maladie(grippé).
maladie(rhume).
maladie(angine).
maladie(bronchite).

% Règles de production
% Si fièvre et toux, alors grippe
production(grippé) :- symptome(fievre), symptome(toux).
% Si mal de gorge et fièvre, alors angine
production(angine) :- symptome(mal_de_gorge), symptome(fievre).
% Si toux et courbatures, alors bronchite
production(bronchite) :- symptome(toux), symptome(courbatures).
% Si seulement fatigue, alors rhume
production(rhume) :- symptome(fatigue).

% But de la requête
requête :-
    % Demander les symptômes
    write("Quels sont vos symptômes ? "), read(Symptoms),
    % Convertir la liste de symptômes en liste de faits
    maplist(symptome, Symptoms, SymptomesFacts),
    % Vérifier les règles de production
    findall(Maladie, (member(production(Maladie), ProductionRules), member(S, SymptomesFacts)), Maladies),
    % Afficher les maladies possibles
    write("Maladies possibles : "), write(Maladies).
```

**Explication du Code**

* **Déclaration des symptômes et des maladies:** Les symptômes et les maladies sont déclarés à l'aide de faits.
* **Règles de production:** Les règles de production déterminent quelles maladies peuvent être déduites à partir des symptômes observés.
* **But de la requête:** La requête est l'entrée principale du système expert. Il demande les symptômes à l'utilisateur et utilise les règles de production pour trouver les maladies possibles.
* **Maplist:** Maplist est utilisé pour convertir une liste de symptômes en une liste de faits de symptômes.
* **findall:** Findall est utilisé pour trouver toutes les maladies qui peuvent être déduites des symptômes observés.
* **Affichage des maladies possibles:** Les maladies possibles sont affichées à l'écran.