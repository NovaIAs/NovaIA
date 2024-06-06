```modula-2
MODULE CRYPTOGRAPHIQUE;

IMPORT "stdlib.m2";
IMPORT Texte;

VAR
    donnees: ARRAY OF CHAR;
    cle: ARRAY OF CHAR;
    result: ARRAY OF CHAR;

PROCEDURE Initialiser(DONNEES: ARRAY OF CHAR; CLE: ARRAY OF CHAR);
BEGIN
    ARRAYLENGTH(donnees) := ARRAYLENGTH(DONNEES);
    ARRAYLENGTH(cle) := ARRAYLENGTH(CLE);
    FOR i TO ARRAYLENGTH(donnees) DO
        donnees[i] := DONNEES[i];
    END;
    FOR i TO ARRAYLENGTH(cle) DO
        cle[i] := CLE[i];
    END;
END Initialiser;

PROCEDURE Chiffrer();
VAR
    i, j: INTEGER;
BEGIN
    FOR i TO ARRAYLENGTH(donnees) DO
        donnees[i] := donnees[i] XOR cle[i MOD ARRAYLENGTH(cle)];
    END;
END Chiffrer;

PROCEDURE Dechiffrer();
VAR
    i, j: INTEGER;
BEGIN
    FOR i TO ARRAYLENGTH(donnees) DO
        donnees[i] := donnees[i] XOR cle[i MOD ARRAYLENGTH(cle)];
    END;
END Dechiffrer;

BEGIN
    // Initialiser les données et la clé
    Initialiser("MESSAGE SECRET", "CLE SECRETE");

    // Chiffrer les données
    Chiffrer();

    // Afficher les données chiffrées
     Texte.EcrireString(donnees);

    // Déchiffrer les données
    Dechiffrer();

    // Afficher les données déchiffrées
    Texte.EcrireString(donnees);
END CRYPTOGRAPHIQUE.
```

**Explication du code :**

Ce code implémente un chiffrement simple XOR en MODULA-2. Voici une explication détaillée :

* **Module `CRYPTOGRAPHIQUE` :** Le code est encapsulé dans un module appelé `CRYPTOGRAPHIQUE`.

* **Procédure `Initialiser` :** Cette procédure initialise les variables `donnees` et `cle` avec les valeurs fournies en paramètres. Elle définit la longueur de ces variables en fonction de la longueur des paramètres.

* **Procédure `Chiffrer` :** Cette procédure chiffre les données en effectuant une opération XOR (`XOR`) entre chaque caractère des données et le caractère correspondant de la clé. La clé est répétée circulairement si nécessaire.

* **Procédure `Dechiffrer` :** Cette procédure déchiffre les données en effectuant à nouveau une opération XOR (`XOR`) entre chaque caractère des données et le caractère correspondant de la clé.

* **Programme principal (bloc `BEGIN...END`) :**
    * Le programme principal appelle la procédure `Initialiser` pour initialiser les données et la clé.
    * Il appelle ensuite la procédure `Chiffrer` pour chiffrer les données.
    * Il affiche les données chiffrées.
    * Il appelle ensuite la procédure `Dechiffrer` pour déchiffrer les données.
    * Il affiche enfin les données déchiffrées.