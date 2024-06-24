```prolog
% Système d'experts pour le diagnostic médical

% Base de connaissances
% La base de connaissances est représentée par un ensemble de faits et de règles.
% Les faits sont des propositions simples qui sont vraies ou fausses.
% Les règles sont des implications de la forme :
%   Si <condition1> et <condition2> et ... et <conditionN> alors <conclusion>

% Faits
patient(pierre).
age(pierre, 35).
sexe(pierre, masculin).
fumeur(pierre, non).
alcoolique(pierre, non).
diabetique(pierre, non).
hypertension(pierre, non).

% Règles
% Le système d'experts utilise des règles pour déduire de nouvelles conclusions à partir des faits connus.
% Les règles sont exprimées à l'aide de la syntaxe suivante :
%   regle(NomRègle, [ListeConditions], Conclusion).

% Règle 1 : Si le patient est un homme, fumeur et alcoolique, alors il a un risque élevé de maladie cardiovasculaire.
regle(risque_cardiovasculaire, [sexe(pierre, masculin), fumeur(pierre, oui), alcoolique(pierre, oui)], risque_eleve).

% Règle 2 : Si le patient est une femme, non fumeuse et non alcoolique, alors elle a un risque faible de maladie cardiovasculaire.
regle(risque_cardiovasculaire, [sexe(pierre, feminin), fumeur(pierre, non), alcoolique(pierre, non)], risque_faible).

% Règle 3 : Si le patient est diabétique, alors il a un risque élevé de neuropathie.
regle(risque_neuropathie, [diabetique(pierre, oui)], risque_eleve).

% Règle 4 : Si le patient est hypertendu, alors il a un risque élevé d'accident vasculaire cérébral.
regle(risque_avc, [hypertension(pierre, oui)], risque_eleve).

% Règle 5 : Si le patient a un risque élevé de maladie cardiovasculaire ou de neuropathie ou d'accident vasculaire cérébral, alors il a un risque global élevé.
regle(risque_global, [risque_cardiovasculaire(risque_eleve); risque_neuropathie(risque_eleve); risque_avc(risque_eleve)], risque_eleve).

% Règle 6 : Sinon, le patient a un risque global faible.
regle(risque_global, [], risque_faible).

% Consultation
% La consultation permet à l'utilisateur de poser des questions au système d'experts.
% L'utilisateur peut poser des questions sur les faits connus ou sur les conclusions déduites.

% Affichage des faits
ecrire('Faits :').
ecrire(' - Patient : ', patient(pierre)).
ecrire(' - Âge : ', age(pierre)).
ecrire(' - Sexe : ', sexe(pierre)).
ecrire(' - Fumeur : ', fumeur(pierre)).
ecrire(' - Alcoolique : ', alcoolique(pierre)).
ecrire(' - Diabétique : ', diabetique(pierre)).
ecrire(' - Hypertension : ', hypertension(pierre)).

% Demande de conclusion
ecrire('Quelle conclusion souhaitez-vous connaître ? (risque_cardiovasculaire, risque_neuropathie, risque_avc, risque_global) :').
lire(Conclusion).

% Recherche de la conclusion dans la base de connaissances
risque(Conclusion, Risque) :-
    regle(Conclusion, Conditions, Risque),
    forall(member(Condition, Conditions),
           (patient(pierre) ->
               call(Condition)
           ;
               Condition = true)).

% Affichage de la conclusion
ecrire('Conclusion : ', Conclusion, ' = ', Risque).
```

**Explications du code**

* La **base de connaissances** est représentée par un ensemble de faits et de règles.
* Les **faits** sont des propositions simples qui sont vraies ou fausses.
* Les **règles** sont des implications de la forme :
    * Si <condition1> et <condition2> et ... et <conditionN> alors <conclusion>
* La **consultation** permet à l'utilisateur de poser des questions au système d'experts.
* L'utilisateur peut poser des questions sur les faits connus ou sur les conclusions déduites.
* Le système d'experts utilise des **règles** pour déduire de nouvelles conclusions à partir des faits connus.
* La **recherche de la conclusion** dans la base de connaissances est effectuée à l'aide du prédicat `risque/3`.
* Le prédicat `risque/3` prend trois arguments :
    * Le premier argument est la conclusion à rechercher.
    * Le deuxième argument est le risque associé à la conclusion.
    * Le troisième argument est une liste de conditions qui doivent être vérifiées pour que la conclusion soit vraie.
* Le **prédi