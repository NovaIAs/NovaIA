### Programme complexe en Modula-2

```modula-2
MODULE ProgrammeComplexe;

FROM InOut IMPORT WriteString, WriteCard, WriteLn, ReadCard;

VAR
  nombreEntier: CARDINAL;

BEGIN
  WriteString("Entrez un nombre entier : ");
  ReadCard(nombreEntier);
  WriteString("Vous avez entré le nombre : ");
  WriteCard(nombreEntier, 0);
  WriteLn();

  IF nombreEntier MOD 2 = 0 THEN
    WriteString("Le nombre est pair");
  ELSE
    WriteString("Le nombre est impair");
  END;
  WriteLn();

  FOR i := 1 TO nombreEntier DO
    IF i MOD 3 = 0 OR i MOD 5 = 0 THEN
      WriteString(i, 0);
      WriteLn();
    END;
  END;
END ProgrammeComplexe.
```

### Explication du code

Ce programme Modula-2 se compose de plusieurs modules :

* **Module `ProgrammeComplexe` :**

  * Ce module est l'entrée du programme. Il contient la logique principale et appelle d'autres modules.

* **Module `InOut` (importé) :**

  * Contient des procédures pour les entrées et sorties standard.

### Fonctions principales du programme :

* Demande à l'utilisateur d'entrer un nombre entier.
* Vérifie si le nombre est pair ou impair et affiche le résultat.
* Affiche tous les nombres de 1 à `nombreEntier` qui sont divisibles par 3 ou 5.

### Explication détaillée :

* **Demande d'entrée :**
  ```modula-2
  WriteString("Entrez un nombre entier : ");
  ReadCard(nombreEntier);
  ```
  Ces lignes affichent un message demandant à l'utilisateur d'entrer un nombre entier, puis lisent l'entrée dans la variable `nombreEntier`.

* **Vérification de parité :**
  ```modula-2
  IF nombreEntier MOD 2 = 0 THEN
    WriteString("Le nombre est pair");
  ELSE
    WriteString("Le nombre est impair");
  END;
  WriteLn();
  ```
  Cette structure conditionnelle vérifie si `nombreEntier` est divisible par 2 (pair) ou non (impair) et affiche le résultat correspondant.

* **Affichage des nombres divisibles par 3 ou 5 :**
  ```modula-2
  FOR i := 1 TO nombreEntier DO
    IF i MOD 3 = 0 OR i MOD 5 = 0 THEN
      WriteString(i, 0);
      WriteLn();
    END;
  END;
  ```
  Cette boucle `FOR` parcourt les nombres de 1 à `nombreEntier`. Pour chaque nombre, elle vérifie s'il est divisible par 3 ou 5 (ou les deux) à l'aide de l'opérateur modulo `MOD`. Si c'est le cas, le nombre est affiché.