**Module principal :**

```modula-2
MODULE Gestion_Compte;
IMPORT Listes, Ordres;

VAR
  comptes : ARRAY[10] OF Compte;
  nbComptes : CARDINAL;

BEGIN
  Demarrer();
  WHILE nbComptes > 0 DO
    GererCompte(comptes[nbComptes]);
    nbComptes := nbComptes - 1
  END;
  Terminer()
END Gestion_Compte.
```

**Module Listes :**

```modula-2
MODULE Listes;

TYPE
  Element = POINTER TO ^Element;

PROCEDURE Ajouter(VAR liste : Element; valeur : Element);
PROCEDURE Supprimer(VAR liste : Element; valeur : Element);
PROCEDURE Chercher(liste : Element; valeur : Element) : BOOLEAN;
```

**Module Ordres :**

```modula-2
MODULE Ordres;

TYPE
  Ordre = RECORD
    numero : CARDINAL;
    montant : REAL
  END;

PROCEDURE CreerOrdre(VAR ordre : Ordre; numero : CARDINAL; montant : REAL);
PROCEDURE AfficherOrdre(ordre : Ordre);
```

**Module Compte :**

```modula-2
MODULE Compte;

TYPE
  Compte = RECORD
    solde : REAL;
    ordres : Listes.Element
  END;

PROCEDURE CreerCompte(VAR compte : Compte; solde : REAL);
PROCEDURE AjouterOrdre(VAR compte : Compte; ordre : Ordres.Ordre);
PROCEDURE SupprimerOrdre(VAR compte : Compte; ordre : Ordres.Ordre);
PROCEDURE CalculerSolde(compte : Compte);
```

**Fonctions :**

```modula-2
PROCEDURE Demarrer();
  VAR i : CARDINAL;
BEGIN
  FOR i TO 10 DO
    comptes[i] := Compte.CreerCompte(100.0)
  END
END Demarrer;

PROCEDURE Terminer();
  VAR i : CARDINAL;
BEGIN
  FOR i TO 10 DO
    Compte.SupprimerCompte(comptes[i])
  END
END Terminer;

PROCEDURE GererCompte(compte : Compte);
BEGIN
  VAR ordre : Ordres.Ordre;
BEGIN
    ordre := Ordres.CreerOrdre(1, 50.0);
    Compte.AjouterOrdre(compte, ordre);
    ordre := Ordres.CreerOrdre(2, 25.0);
    Compte.AjouterOrdre(compte, ordre);
    Compte.CalculerSolde(compte)
  END
END GererCompte;
```

**Explication :**

Ce code implémente un système de gestion de comptes bancaires. Le module principal crée un tableau de 10 comptes et les initialise avec un solde de 100 €. Il boucle ensuite sur les comptes et demande à l'utilisateur d'effectuer des opérations (dépôts, retraits).

Les modules Listes et Ordres fournissent des types de données et des procédures pour gérer les listes et les ordres. Le module Compte définit le type de données compte et les procédures pour gérer les comptes.

Les fonctions Demarrer() et Terminer() initialisent et nettoient respectivement le système. La fonction GererCompte() gère les opérations sur un compte.