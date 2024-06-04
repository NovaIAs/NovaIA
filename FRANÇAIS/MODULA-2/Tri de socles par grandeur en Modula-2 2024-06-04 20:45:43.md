```modula-2
MODULE Socles;
FROM InOut IMPORT WriteString, WriteLn;

VAR NbSocle   : CARDINAL;          (* Nombre de socles *)
    Grandeurs : ARRAY[0..15] OF REAL; (* Tableau de grandeurs *)
    Socle     : ARRAY[0..15] OF CARDINAL; (* Tableau de socles *)

PROCEDURE Saisie;
BEGIN
  WriteString("Combien de socles souhaitez-vous ? ");
  WriteString("Entrez un nombre compris entre 1 et 16 : ");
  NbSocle := ReadCard;
  WHILE (NbSocle < 1) OR (NbSocle > 16) DO
    WriteString("Erreur : le nombre de socles doit être compris entre 1 et 16.");
    WriteString("Réentrez un nombre valide : ");
    NbSocle := ReadCard;
  END;
END Saisie;

PROCEDURE Remplissage;
VAR I : CARDINAL;
BEGIN
  FOR I := 0 TO NbSocle - 1 DO
    WriteString("Entrez la grandeur du socle n°" + I + ": ");
    Grandeurs[I] := ReadReal;
  END;
END Remplissage;

PROCEDURE TriParSocle;
VAR I, J, Min : CARDINAL;
    TGrandeurs, TSocle : ARRAY[0..15] OF REAL; (* Tableaux temporaires *)
BEGIN
  FOR I := 0 TO NbSocle - 1 DO
    TGrandeurs[I] := Grandeurs[I];
    TSocle[I] := Socle[I];
  END;
  FOR I := 0 TO NbSocle - 2 DO
    Min := I;
    FOR J := I + 1 TO NbSocle - 1 DO
      IF TGrandeurs[J] > TGrandeurs[Min] THEN
        Min := J;
      END;
    IF Min <> I THEN
      TGrandeurs[Min] := Grandeurs[I];
      TGrandeurs[I] := Grandeurs[Min];
      TSocle[Min] := Socle[I];
      TSocle[I] := Socle[Min];
    END;
  END;
  FOR I := 0 TO NbSocle - 1 DO
    Grandeurs[I] := TGrandeurs[I];
    Socle[I] := TSocle[I];
  END;
END TriParSocle;

PROCEDURE Affichage;
VAR I : CARDINAL;
BEGIN
  WriteString("Voici la liste des socles triée par grandeur décroissante :\n");
  FOR I := 0 TO NbSocle - 1 DO
    WriteString("Socle n°" + I + " : grandeur = " + Grandeurs[I] + ", numéro = " + Socle[I] + "\n");
  END;
END Affichage;

BEGIN
  Saisie;
  Remplissage;
  TriParSocle;
  Affichage;
END Socles.
```

**Explication du code :**

Ce code MODULA-2 est un programme qui permet de trier des socles en fonction de leur grandeur. Il comporte les procédures suivantes :

* **Saisie:** Demande à l'utilisateur d'entrer le nombre de socles à trier.
* **Remplissage:** Demande à l'utilisateur d'entrer la grandeur de chaque socle.
* **TriParSocle:** Tri des socles en fonction de leur grandeur décroissante.
* **Affichage:** Affiche la liste des socles triés par grandeur décroissante.

Le code utilise les tableaux suivants :

* **Grandeurs:** Tableau contenant les grandeurs des socles.
* **Socle:** Tableau contenant les numéros des socles.

Le tri est effectué à l'aide de l'algorithme de tri par sélection, qui consiste à trouver la plus petite grandeur restante et à l'échanger avec la première grandeur du tableau. Ce processus est répété jusqu'à ce que toutes les grandeurs soient triées.