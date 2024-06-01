**Module Texte**

```modula-2
MODULE Texte;

IMPORT
    Entiers,
    Stdio;

VAR
    texte: ARRAY 80 OF CHAR;
    longueur: CARDINAL;
    i: CARDINAL;

PROCEDURE SaisirTexte;
BEGIN
    Write("Entrez un texte :");
    Read(texte);
    longueur := Length(texte);
END SaisirTexte;

PROCEDURE AfficherTexte;
BEGIN
    Write("Texte saisi :");
    Write(texte);
END AfficherTexte;

PROCEDURE AnalyserTexte;
VAR
    nbVoyelles, nbConsonnes: CARDINAL;
BEGIN
    nbVoyelles := 0;
    nbConsonnes := 0;
    FOR i := 1 TO longueur DO
        CASE texte[i] OF
            'a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U':
                nbVoyelles := nbVoyelles + 1;
            ELSE:
                nbConsonnes := nbConsonnes + 1;
        END;
    END;
    Write("Nombre de voyelles :");
    Write(nbVoyelles);
    Write("Nombre de consonnes :");
    Write(nbConsonnes);
END AnalyserTexte;

BEGIN
    SaisirTexte;
    AfficherTexte;
    AnalyserTexte;
END Texte.
```

**Explication du code :**

Ce code implémente un petit programme qui permet de saisir un texte, de l'afficher et de compter le nombre de voyelles et de consonnes qu'il contient.

Le module **Texte** contient :

* Trois variables :
    * **texte** : un tableau de caractères pour stocker le texte saisi.
    * **longueur** : un entier cardinal pour stocker la longueur du texte.
    * **i** : un entier cardinal utilisé comme compteur.
* Trois procédures :
    * **SaisirTexte** : permet de saisir le texte auprès de l'utilisateur.
    * **AfficherTexte** : permet d'afficher le texte saisi.
    * **AnalyserTexte** : analyse le texte saisi et compte le nombre de voyelles et de consonnes.

La procédure principale du programme :

* Appelle **SaisirTexte** pour obtenir le texte de l'utilisateur.
* Appelle **AfficherTexte** pour afficher le texte saisi.
* Appelle **AnalyserTexte** pour analyser le texte et afficher le nombre de voyelles et de consonnes.