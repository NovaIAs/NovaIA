**Code MODULA-2 complexe**

```modula-2
MODULE SystèmesTechniques;
IMPORT Bases, Affichage, Texte;

DEFINE
  type TCapteur = CARDINAL;
  type TActionneur = CARDINAL;
  type T SystemeBase = (
    capteurs: ARRAY TCapteur OF BOOLEAN;
    actionneurs: ARRAY TActionneur OF BOOLEAN;
    liaison: ARRAY [TCapteur, TActionneur] OF BOOLEAN
  );

PROCEDURE EcrireDonnéeBooléenne(texte: ARRAY OF CHAR; valeur: BOOLEAN);

BEGIN
  (* Affiche une valeur booléenne *)
  IF valeur THEN
    Affichage.Écrire("True");
  ELSE
    Affichage.Écrire("False");
  END;
END EcrireDonnéeBooléenne;

PROCEDURE ÉcrireSystème(système: T SystemeBase);

VAR
  i, j: CARDINAL;
BEGIN
  (* Affiche l'état d'un système *)
  Texte.ÉcrireLigne("Capteurs :");
  FOR i TO HIGH(système.capteurs) DO
    Texte.Écrire(i, ": ");
    EcrireDonnéeBooléenne(système.capteurs[i]);
    Texte.ÉcrireLigne;
  END;

  Texte.ÉcrireLigne("Actionneurs :");
  FOR i TO HIGH(système.actionneurs) DO
    Texte.Écrire(i, ": ");
    EcrireDonnéeBooléenne(système.actionneurs[i]);
    Texte.ÉcrireLigne;
  END;

  Texte.ÉcrireLigne("Liaisons :");
  FOR i TO HIGH(système.capteurs) DO
    FOR j TO HIGH(système.actionneurs) DO
      IF système.liaison[i, j] THEN
        Texte.Écrire("Capteur ", i, " -> Actionneur ", j, "  ");
      END;
    END;
    Texte.ÉcrireLigne;
  END;
END ÉcrireSystème;

PROCEDURE CréerSystème();

VAR
  système: T SystemeBase;
BEGIN
  (* Crée un nouveau système *)
  système.capteurs := (True, False, True, False);
  système.actionneurs := (False, True, False, True);
  système.liaison := ((True, True, False, False),
                        (False, True, False, False),
                        (False, False, True, True),
                        (False, True, False, True));
  ÉcrireSystème(système);
END CréerSystème;

BEGIN
  CréerSystème();
END SystèmesTechniques.
```

**Explication du code**

Ce code complexe MODULA-2 implémente un système technique avec des capteurs, des actionneurs et des liaisons entre eux.

**Types de données**

* **TCapteur** et **TActionneur** sont des types énumérés qui représentent les capteurs et les actionneurs.
* **TSystèmeBase** est un enregistrement qui stocke l'état (vrai ou faux) de chaque capteur et actionneur, ainsi que les liaisons entre eux.

**Procédures**

* **EcrireDonnéeBooléenne** affiche une valeur booléenne sous forme de texte "True" ou "False".
* **ÉcrireSystème** affiche l'état complet d'un système, y compris les états des capteurs, des actionneurs et des liaisons.
* **CréerSystème** crée un nouveau système avec des états et des liaisons spécifiques.

**Fonctionnalités**

Le code permet de créer et d'afficher l'état d'un système technique, avec des informations sur les capteurs, les actionneurs et les liaisons. Ce système pourrait être utilisé pour simuler ou contrôler un système réel, tel qu'une chaîne de production.