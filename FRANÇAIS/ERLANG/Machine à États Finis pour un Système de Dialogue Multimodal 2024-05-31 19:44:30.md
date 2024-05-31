```erlang
%% Ce module implémente une machine à états finis (MEF) pour un système de dialogue multimodal.

-module(mef).
-export([demarrer/0, etat_initial/0, evenements/0]).

%% Fonctions d'état

etat_initial() ->
    {etat_dialogue,
     {langue, francais},
     {mode, vocal},
     {historique, []}}.

etat_dialogue(Langue, Mode, Historique) ->
    receive
        {evenement, message, Message} ->
            {etat_dialogue, Langue, Mode, Historique ++ [Message]};
        {evenement, changer_langue, NouvelleLangue} ->
            {etat_dialogue, NouvelleLangue, Mode, Historique};
        {evenement, changer_mode, NouveauMode} ->
            {etat_dialogue, Langue, NouveauMode, Historique};
        {evenement, quitter} ->
            {etat_attente_confirmation, Langue, Mode, Historique}
    end.

etat_attente_confirmation(Langue, Mode, Historique) ->
    receive
        {evenement, confirmer} ->
            {etat_dialogue, Langue, Mode, Historique};
        {evenement, annuler} ->
            {etat_dialogue, Langue, Mode, Historique};
        {evenement, quitter} ->
            exit(normal)
    end.

%% Fonctions d'événements

evenements() ->
    receive
        {evenement, message, _} ->
            evenements();
        {evenement, changer_langue, _} ->
            evenements();
        {evenement, changer_mode, _} ->
            evenements();
        {evenement, quitter} ->
            evenements();
        {evenement, confirmer} ->
            evenements();
        {evenement, annuler} ->
            evenements()
    end.

%% Fonction principale

demarrer() ->
    Etat = etat_initial(),
    spawn(mef, evenements, []),
    loop(Etat).

loop(Etat) ->
    receive
        {evenement, E} ->
            {EtatSuivant, _} = Etat,
            loop(EtatSuivant);
        {etat, EtatSuivant} ->
            loop(EtatSuivant)
    end.
```

### Explication du code

Ce code implémente une machine à états finis (MEF) pour un système de dialogue multimodal.

**Fonctions d'état**

* `etat_initial` : Renvoie l'état initial de la MEF, avec la langue définie sur le français, le mode sur vocal et l'historique vide.
* `etat_dialogue` : Gère les événements reçus dans l'état de dialogue. Il peut modifier la langue, le mode ou l'historique en fonction de l'événement reçu.
* `etat_attente_confirmation` : Gère les événements reçus dans l'état d'attente de confirmation. Il peut confirmer ou annuler l'action en cours.

**Fonctions d'événements**

* `evenements` : Gère les événements reçus par la MEF. Il reboucle sur lui-même pour traiter les événements suivants.

**Fonction principale**

* `demarrer` : Démarre la MEF en créant un processus pour gérer les événements et en appelant la fonction `loop` avec l'état initial.
* `loop` : Boucle principale de la MEF. Elle reçoit les événements et modifie l'état en fonction des événements reçus.