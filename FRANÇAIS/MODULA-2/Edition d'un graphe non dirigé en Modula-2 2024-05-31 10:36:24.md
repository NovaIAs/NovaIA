```modula-2
MODULE Graphe;

IMPORT
  INOUT, ORD_STRING;

VAR
  sommets : ARRAY [1..MAX_SOMMETS] OF type_sommet;
  nb_sommets : INTEGER;

PROCEDURE Main;
BEGIN
  WHILE TRUE DO
    Print("Commande (A=ajouter, S=supprimer, V=visiter, Q=quitter) : ");
    CASE INOUT.InChar OF
      'A': AjouterSommet;
      'S': SupprimerSommet;
      'V': VisiterGraphe;
      'Q': EXIT;
      ELSE: PrintLine("Commande invalide");
    END;
  END;
END Main.

PROCEDURE AjouterSommet;
VAR
  nouveau_sommet : type_sommet;
BEGIN
  PrintLine("Nom du sommet (maximum 10 caractères) : ");
  nouveau_sommet.nom := ORD_STRING.ToUpper(INOUT.InString(10));
  IF nouveau_sommet.nom = "" THEN RETURN; END;
  sommets[nb_sommets] := nouveau_sommet;
  Inc(nb_sommets);
END AjouterSommet.

PROCEDURE SupprimerSommet;
VAR
  i : INTEGER;
  nom_sommet : ORD_STRING;
BEGIN
  PrintLine("Nom du sommet à supprimer : ");
  nom_sommet := ORD_STRING.ToUpper(INOUT.InString(10));
  IF nom_sommet = "" THEN RETURN; END;
  FOR i := 1 TO nb_sommets DO
    IF sommets[i].nom = nom_sommet THEN
      Dec(nb_sommets);
      sommets[i] := sommets[nb_sommets];
      EXIT;
    END;
  END;
  PrintLine("Sommet non trouvé");
END SupprimerSommet.

PROCEDURE VisiterGraphe;
VAR
  i : INTEGER;
  file : ARRAY [1..MAX_SOMMETS] OF type_sommet;
  debut_file, fin_file : INTEGER;
BEGIN
  debut_file := fin_file := 0;
  file[fin_file] := sommets[1];
  Inc(fin_file);
  WHILE debut_file < fin_file DO
    Print(file[debut_file].nom);
    FOR i := 1 TO sommets[debut_file].nb_voisins DO
      IF NOT sommets[debut_file].voisins[i].visited THEN
        file[fin_file] := sommets[debut_file].voisins[i];
        Inc(fin_file);
      END;
    END;
    sommets[debut_file].visited := TRUE;
    Inc(debut_file);
  END;
  PrintLine;
  FOR i := 1 TO nb_sommets DO
    sommets[i].visited := FALSE;
  END;
END VisiterGraphe.

TYPE
  type_sommet = RECORD
    nom : ARRAY [1..10] OF CHAR; (* Nom du sommet *)
    voisins : ARRAY [1..MAX_VOISINS] OF type_voisin; (* Voisins du sommet *)
    nb_voisins : INTEGER;
    visited : BOOLEAN; (* Indique si le sommet a été visité *)
  END;

  type_voisin = RECORD
    REF sommet : type_sommet;
    poids : INTEGER;
  END;

CONST
  MAX_SOMMETS = 100; (* Nombre maximum de sommets dans le graphe *)
  MAX_VOISINS = 100; (* Nombre maximum de voisins pour un sommet *)

```

**Explication du code :**

Ce programme Modula-2 implémente un graphe non dirigé. Il permet d'ajouter, de supprimer et de visiter des sommets (nœuds) dans le graphe.

**Structure du graphe :**

Le graphe est représenté par un tableau `sommets` de structures `type_sommet`. Chaque structure `type_sommet` représente un sommet du graphe et contient :
- `nom` : un tableau de caractères qui stocke le nom du sommet.
- `voisins` : un tableau de structures `type_voisin` qui stocke les sommets voisins.
- `nb_voisins` : le nombre de voisins du sommet.
- `visited` : un booléen qui indique si le sommet a été visité lors d'un parcours.

**Procédure `Main` :**

La procédure `Main` est le point d'entrée du programme. Elle affiche un menu à l'utilisateur qui lui permet de choisir entre les opérations suivantes :
- A : Ajouter un sommet
- S : Supprimer un sommet
- V : Visiter le graphe
- Q : Quitter

**Procédure `AjouterSommet` :**

Cette procédure permet d'ajouter un nouveau sommet au graphe. Elle demande un nom de sommet à l'utilisateur et l'ajoute au tableau `sommets` s'il n'est pas vide.

**Procédure `SupprimerSommet` :**

Cette procédure permet de supprimer un sommet du graphe. Elle demande un nom de sommet à l'utilisateur et le supprime du tableau `sommets` s'il existe.

**Procédure `VisiterGraphe` :**

Cette procédure effectue un parcours en largeur du graphe. Elle utilise une file pour parcourir les sommets. Elle commence par placer le premier sommet dans la file, puis parcourt tous ses voisins.

**Constantes et types :**

Le programme définit les constantes `MAX_SOMMETS` et `MAX_VOISINS` pour spécifier les limites du graphe. Il définit également le type `type_voisin` qui représente un sommet voisin avec son poids.