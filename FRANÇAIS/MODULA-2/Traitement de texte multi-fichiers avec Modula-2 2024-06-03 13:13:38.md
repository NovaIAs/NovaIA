```modula-2
MODULE MultiplesFichiers;
FROM InOut IMPORT ReadLine;
FROM Strings IMPORT String, GetLength;

VAR
  texte: String;

PROCEDURE Traitertexte(texte: String);
BEGIN
  WriteString("Texte original : ", texte);
  WriteLn;
  texte := Replace(texte, " ", "_");
  WriteString("Texte avec les espaces remplacés par des underscores : ", texte);
  WriteLn;
END Traitertexte;

PROCEDURE ChargerTexte(texte: VAR String);
VAR
  filename: String;
BEGIN
  Write("Nom du fichier à charger : ");
  ReadLine(filename);
  IF OpenInput(filename) THEN
    texte := Read(filename, GetLength(filename));
    CloseInput(filename);
  ELSE
    WriteString("Erreur lors de l'ouverture du fichier ", filename);
    WriteLn;
  END;
END ChargerTexte;

PROCEDURE SauverTexte(texte: String);
VAR
  filename: String;
BEGIN
  Write("Nom du fichier à sauvegarder : ");
  ReadLine(filename);
  IF OpenOutput(filename) THEN
    WriteString(filename, texte);
    CloseOutput(filename);
    WriteString("Texte sauvegardé dans le fichier ", filename);
    WriteLn;
  ELSE
    WriteString("Erreur lors de l'ouverture du fichier ", filename);
    WriteLn;
  END;
END SauverTexte;

BEGIN
  WriteLine("Bienvenue dans l'application de traitement de texte !");
  
  texte := "";
  ChargerTexte(texte);
  Traitertexte(texte);
  SauverTexte(texte);
  
  Write("Appuyez sur une touche pour quitter...");
  ReadLine();
END MultiplesFichiers.
```

**Explication du code :**

Ce code MODULA-2 est un exemple d'application de traitement de texte simple. Voici une explication détaillée :

* **Module principal** : Le module principal `MultiplesFichiers` définit le point d'entrée du programme.

* **Procédures** :
    * `TraiterTexte` : Cette procédure prend une chaîne de caractères `texte` en argument et remplace les espaces par des underscores.
    * `ChargerTexte` : Cette procédure permet de charger un fichier texte dans la variable `texte`.
    * `SauverTexte` : Cette procédure permet de sauvegarder la chaîne `texte` dans un fichier texte.

* **Variables** :
    * `texte` : Une variable de type chaîne de caractères utilisée pour stocker le texte à traiter.

* **Fonctions d'entrée/sortie** :
    * `ReadLine` : Permet de lire une ligne de texte depuis l'entrée standard.
    * `OpenInput` et `CloseInput` : Permettent d'ouvrir et de fermer un fichier texte en lecture.
    * `OpenOutput` et `CloseOutput` : Permettent d'ouvrir et de fermer un fichier texte en écriture.
    * `WriteString` et `Write` : Permettent d'écrire une chaîne de caractères dans un fichier ou sur la sortie standard.

* **Déroulement du programme** :
    1. L'utilisateur est invité à saisir le nom d'un fichier texte à charger.
    2. La fonction `ChargerTexte` lit le fichier et charge son contenu dans la variable `texte`.
    3. La fonction `TraiterTexte` remplace les espaces dans `texte` par des underscores.
    4. L'utilisateur est invité à saisir le nom d'un fichier dans lequel sauvegarder le texte traité.
    5. La fonction `SauverTexte` écrit le contenu de `texte` dans le fichier spécifié.