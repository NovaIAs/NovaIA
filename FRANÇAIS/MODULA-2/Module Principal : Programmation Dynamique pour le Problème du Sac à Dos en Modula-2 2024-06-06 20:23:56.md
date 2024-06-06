**Module Principal : Programmation Dynamique pour le Problème du Sac à Dos**

```modula-2
MODULE KnapsackProgram;
FROM InOut IMPORT WriteString, WriteLn, WriteInt, ReadInt;
FROM Sequence IMPORT Array;

IMPORT Sort, System;

TYPE
  ItemRecord = RECORD
    weight  : LONGINT;
    value   : LONGINT;
  END;

  ItemArray = Array[0..MAX_ITEMS] OF ItemRecord;
  TableArray = Array[0..MAX_WEIGHT, 0..MAX_ITEMS] OF LONGINT;

  PROC SelectItems (value : LONGINT; itemWeight : LONGINT;
                    itemValue : LONGINT; VAR selected : CARDINAL);

BEGIN
  IF value < itemWeight THEN
    RETURN;
  END;

  FOR i := selected DOWNTO 0 DO
    IF weight[selected] + itemWeight <= value THEN
      value := value - itemWeight;
      selected := selected - 1;
    ELSE
      RETURN;
    END;
  END;
END SelectItems;

PROC CalculateTable (weight : LONGINT; itemArray : ItemArray; VAR table : TableArray);
VAR
  i, w, v : CARDINAL;
BEGIN
  FOR w := 0 TO weight DO
    table[w, 0] := 0;
    FOR i := 1 TO MAX_ITEMS DO
      v := itemArray[i].value;
      IF v > 0 THEN
        FOR w := weight DOWNTO v DO
          table[w, i] := MAX(table[w, i], table[w - v, i - 1] + v);
        END;
      END;
    END;
  END;
END CalculateTable;

PROC PrintSelectedItems (itemArray : ItemArray; weight : LONGINT; table : TableArray);
VAR
  value, i, j : CARDINAL;
  selected := MAX_ITEMS;
BEGIN
  value := table[weight, MAX_ITEMS];
  WriteString("Items sélectionnés : ");
  FOR j := MAX_ITEMS DOWNTO 1 DO
    SelectItems(value, itemArray[j].weight, itemArray[j].value, selected);
    IF selected <> 0 THEN
      WriteInt(j, 0);
      value := value - itemArray[j].value;
      IF value = 0 THEN
        RETURN;
      END;
    END;
  END;
  WriteString("AUCUN ITEM NE PEUT ÊTRE SÉLECTIONNÉ !");
END PrintSelectedItems;

VAR
  weight : LONGINT;
  itemArray : ItemArray;
  solutionTable : TableArray;
  weightCount, i, value : CARDINAL;

BEGIN
  WriteString("Entrez le poids maximum du sac à dos : ");
  weight := ReadInt();
  WriteString("Entrez le nombre d'objets : ");
  weightCount := ReadInt();

  FOR i := 1 TO weightCount DO
    WriteString("Entrez le poids de l'objet ", i, 0);
    itemArray[i].weight := ReadInt();
    WriteString("Entrez la valeur de l'objet ", i, 0);
    itemArray[i].value := ReadInt();
  END;

  WriteString("Calcul du tableau de solutions...");
  CalculateTable(weight, itemArray, solutionTable);

  value := solutionTable[weight, MAX_ITEMS];
  IF value = 0 THEN
    WriteString("AUCUNE SOLUTION N'EST POSSIBLE !");
    EXIT PROGRAM;
  END;

  WriteString("Valeur maximale obtenue : ", value, 0);
  WriteLn();

  WriteString("Objets sélectionnés : ");
  PrintSelectedItems(itemArray, weight, solutionTable);

  WriteString("Appuyez sur une touche pour quitter...");
  ReadChar();
END KnapsackProgram.
```

**Explication du Code :**

Ce code implémente l'algorithme de programmation dynamique pour résoudre le problème du sac à dos.

**Structure de Données :**

* **ItemRecord** : Enregistrement représentant un objet avec son poids et sa valeur.
* **ItemArray** : Tableau d'objets.
* **TableArray** : Tableau bidimensionnel représentant le tableau de solutions.

**Fonctions :**

* **SelectItems** : Sélectionne les objets à inclure dans le sac à dos.
* **CalculateTable** : Calcule le tableau de solutions à l'aide de la relation de récurrence.
* **PrintSelectedItems** : Imprime les objets sélectionnés qui donnent la valeur maximale.

**Variables Principales :**

* **weight** : Poids maximum du sac à dos.
* **itemArray** : Tableau d'objets.
* **solutionTable** : Tableau de solutions.
* **weightCount** : Nombre d'objets.

**Déroulement du Programme :**

1. Demande le poids maximum du sac à dos et le nombre d'objets.
2. Crée un tableau d'objets avec leurs poids et valeurs.
3. Calcule le tableau de solutions à l'aide de la programmation dynamique.
4. Trouve la valeur maximale obtenue.
5. Imprime les objets sélectionnés qui donnent la valeur maximale.