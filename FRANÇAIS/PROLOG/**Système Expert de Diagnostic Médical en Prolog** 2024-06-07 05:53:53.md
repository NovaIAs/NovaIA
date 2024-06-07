**Système d'Expert de Diagnostic Médical**

```prolog
/* Déclaration des faits de base */
fait(grippe, symptome(fievre)).
fait(grippe, symptome(toux)).
fait(grippe, symptome(courbatures)).
fait(rhume, symptome(nez_qui_coule)).
fait(rhume, symptome(eternuements)).
fait(rhume, symptome(mal_de_gorge)).
fait(bronchite, symptome(essoufflement)).
fait(bronchite, symptome(toux_grasse)).
fait(bronchite, symptome(expectorations)).

/* Règle de production pour le diagnostic de la grippe */
regle(grippe) :-
    fait(grippe, S1),
    fait(grippe, S2),
    S1 \== S2,
    membre(S1, [fievre, toux, courbatures]),
    membre(S2, [fievre, toux, courbatures]).

/* Règle de production pour le diagnostic du rhume */
regle(rhume) :-
    fait(rhume, S1),
    fait(rhume, S2),
    S1 \== S2,
    membre(S1, [nez_qui_coule, eternuements, mal_de_gorge]),
    membre(S2, [nez_qui_coule, eternuements, mal_de_gorge]).

/* Règle de production pour le diagnostic de la bronchite */
regle(bronchite) :-
    fait(bronchite, S1),
    fait(bronchite, S2),
    S1 \== S2,
    membre(S1, [essoufflement, toux_grasse, expectorations]),
    membre(S2, [essoufflement, toux_grasse, expectorations]).

/* Fonction de demande d'un symptôme */
demande_symptome(Symptome) :-
    ecrire("Quel est le symptôme ? "),
    lire(Symptome).

/* Fonction d'affichage du diagnostic */
affiche_diagnostic(Diagnostic) :-
    ecrire("Le diagnostic est : "),
    ecrire(Diagnostic),
    ecrire("\n").

/* Boucle principale du système d'expert */
main :-
    repeat,
    demande_symptome(Symptome),
    (
        Symptome = 'fin' ->
            nl,
            ecrire("Au revoir !"),
            halt
        ;
        regle(Diagnostic),
        affiche_diagnostic(Diagnostic)
    ).
```

**Explication du code**

Ce code PROLOG implémente un système d'expert de diagnostic médical simple. Il diagnostique trois maladies courantes : la grippe, le rhume et la bronchite, en fonction des symptômes saisis par l'utilisateur.

**Faits de base**

Les faits de base sont des déclarations de faits connus. Dans ce cas, ils décrivent les symptômes associés à chaque maladie.

**Règles de production**

Les règles de production sont des règles logiques qui permettent au système d'inférer un diagnostic à partir des symptômes. Elles ont la forme "si-alors", où la partie "si" décrit les symptômes et la partie "alors" décrit la maladie diagnostiquée.

**Fonctionnalités du système**

Le système demande à l'utilisateur de saisir des symptômes un par un. Il utilise ensuite les règles de production pour diagnostiquer la maladie la plus probable. L'utilisateur peut saisir "fin" pour quitter le système.

**Avantages de PROLOG**

PROLOG est bien adapté au développement de systèmes d'expert car :

* Il fournit une représentation déclarative des connaissances.
* Il prend en charge la séparation entre les faits et les règles.
* Il utilise une inférence en arrière-chaînage, qui est bien adaptée à la résolution de problèmes.