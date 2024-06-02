**Module de tri d'une liste d'entiers en ordre croissant**

```modula-2
(* Trie une liste d'entiers en ordre croissant *)

MODULE TriListe;

IMPORT Out, In, List;

TYPE
    Entier = CARDINAL;
    ListeEntiers = ARRAY OF Entier;

PROCEDURE TriInsertion(VAR l: ListeEntiers);
VAR
    i, j, tmp: Entier;
BEGIN
    FOR i := 2 TO List.haut(l) DO
        tmp := l[i];
        j := i - 1;
        WHILE (j > 0) AND (l[j] > tmp) DO
            l[j + 1] := l[j];
            j := j - 1
        END;
        l[j + 1] := tmp
    END
END TriInsertion;

PROCEDURE AfficheListe(l: ListeEntiers);
VAR
    i: Entier;
BEGIN
    FOR i := List.bas(l) TO List.haut(l) DO
        Out.int(l[i], 0)
    END;
    Out.ln()
END AfficheListe;

PROCEDURE SaisieListe(VAR l: ListeEntiers);
VAR
    i: Entier;
    c: CHAR;
BEGIN
    List.init(l, 10);
    i := List.bas(l);
    WHILE (i <= List.haut(l)) AND NOT (In.char(c) = #\10) DO
        l[i] := In.int(c, 0);
        i := i + 1
    END;
    List.set_haut(l, i - 1)
END SaisieListe;

BEGIN
    VAR
        liste: ListeEntiers;
    BEGIN
        SaisieListe(liste);
        TriInsertion(liste);
        AfficheListe(liste)
    END
END TriListe.
```

**Explication du code**

Ce code implémente un algorithme de tri en insertion qui trie une liste d'entiers en ordre croissant.

**Module principal**

* `TriListe` : Le module principal définit les types, les procédures et la fonction principale.

**Types**

* `Entier` : Type représentant un entier sans signe.
* `ListeEntiers` : Type représentant une liste d'entiers.

**Procédures**

* `TriInsertion` : Procède au tri d'une liste d'entiers en ordre croissant en utilisant l'algorithme du tri par insertion.
* `AfficheListe` : Affiche les éléments d'une liste d'entiers sur la sortie standard.
* `SaisieListe` : Saisit les éléments d'une liste d'entiers à partir de l'entrée standard.

**Fonction principale**

* `BEGIN ... END TriListe.` : La fonction principale appelle les procédures `SaisieListe` et `TriInsertion` pour saisir une liste d'entiers, la trier et afficher le résultat.

**Fonctionnement de l'algorithme de tri par insertion**

La procédure `TriInsertion` implémente l'algorithme de tri par insertion comme suit :

1. Elle parcourt la liste de l'élément 2 jusqu'à la fin.
2. Pour chaque élément, elle le compare aux éléments précédents jusqu'à trouver sa position correcte dans la liste.
3. Elle décale tous les éléments situés après la position correcte d'une position vers la droite.
4. Elle insère l'élément à sa position correcte.

**Utilisation**

Ce module peut être utilisé comme suit :

```
MODULE Utilisateur;
FROM TriListe IMPORT TriInsertion, AfficheListe, SaisieListe;

VAR
    liste: ListeEntiers;

BEGIN
    SaisieListe(liste);
    TriInsertion(liste);
    AfficheListe(liste)
END Utilisateur.
```