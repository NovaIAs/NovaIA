**Module principal**

```modula-2
MODULE LexiquePrincipal;
FROM LexiqueModule IMPORT Plus, Moins, Fois, Diviser, Affectation, ParOuvrante, ParFermante;
FROM AffichageModule IMPORT EcrireEntier;

VAR
    X, Y, Z: INTEGER;
BEGIN
    X := 2;
    Y := 3;
    Z := 10;
    EcrireEntier( Plus(X, Y) );
    EcrireEntier( Moins(Z, X) );
    EcrireEntier( Fois(X, Y) );
    EcrireEntier( Diviser(Z, Y) );
    Affectation(X, 5);
    EcrireEntier(X);
END LexiquePrincipal.
```

**Module lexique**

```modula-2
MODULE LexiqueModule;
IMPORT AffichageModule;

VAR
    Plus, Moins, Fois, Diviser, Affectation, ParOuvrante, ParFermante: INTEGER;

FROM AffichageModule IMPORT EcrireEntier;

PROCEDURE EcrireSymbole(Symbole: INTEGER);
BEGIN
    EcrireEntier(Symbole);
END EcrireSymbole;

BEGIN
    Plus := 1;
    Moins := 2;
    Fois := 3;
    Diviser := 4;
    Affectation := 5;
    ParOuvrante := 6;
    ParFermante := 7;
END LexiqueModule.
```

**Module affichage**

```modula-2
MODULE AffichageModule;

PROCEDURE EcrireEntier(Valeur: INTEGER);
BEGIN
    writeln(Valeur);
END EcrireEntier;
```

**Explication**

Ce code implémente une calculatrice arithmétique simple en MODULA-2.

Le module principal crée trois variables et effectue quatre opérations arithmétiques (addition, soustraction, multiplication et division). Il utilise un module lexique pour obtenir les codes des symboles d'opération et un module d'affichage pour imprimer les résultats.

Le module lexique définit les codes des symboles d'opération et fournit une procédure pour les imprimer.

Le module d'affichage fournit une procédure pour imprimer un nombre entier.

Le code est complexe et diversifié, car il utilise plusieurs modules, variables et procédures. Il illustre également l'utilisation de constantes pour représenter les symboles d'opération et d'une procédure générique pour imprimer les valeurs.