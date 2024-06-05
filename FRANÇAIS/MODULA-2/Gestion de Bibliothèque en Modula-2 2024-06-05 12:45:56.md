**Module de Gestion d'une Bibliothèque**

```modula-2
MODULE Bibliothèque;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn, WriteChar;
FROM InOut IMPORT ReadChar, ReadCard;

VAR
    livres: ARRAY [1..100] OF RECORD
        titre: STRING[50];
        auteur: STRING[50];
        année: CARDINAL;
        disponible: BOOLEAN;
    END;
    nbLivres: CARDINAL;

PROCEDURE AjouterLivre;
VAR
    i: INTEGER;
BEGIN
    WriteString("Entrez le titre du livre : ");
    ReadString(livres[nbLivres + 1].titre);
    WriteString("Entrez l'auteur du livre : ");
    ReadString(livres[nbLivres + 1].auteur);
    WriteString("Entrez l'année de publication : ");
    ReadCard(livres[nbLivres + 1].année);
    livres[nbLivres + 1].disponible := TRUE;
    Inc(nbLivres);
END AjouterLivre;

PROCEDURE RechercherLivre;
VAR
    titre: STRING[50];
    i: INTEGER;
    trouvé: BOOLEAN;
BEGIN
    WriteString("Entrez le titre du livre à rechercher : ");
    ReadString(titre);
    trouvé := FALSE;
    FOR i := 1 TO nbLivres DO
        IF livres[i].titre = titre THEN
            trouvé := TRUE;
            WriteString("Livre trouvé : ");
            WriteString(livres[i].titre);
            WriteChar(' ');
            WriteString(livres[i].auteur);
            WriteLn();
            IF livres[i].disponible THEN
                WriteString("Disponible")
            ELSE
                WriteString("Emprunté")
            END;
            WriteLn();
        END;
    END;
    IF NOT trouvé THEN
        WriteString("Livre non trouvé")
    END;
END RechercherLivre;

PROCEDURE EmprunterLivre;
VAR
    titre: STRING[50];
    i: INTEGER;
    trouvé: BOOLEAN;
BEGIN
    WriteString("Entrez le titre du livre à emprunter : ");
    ReadString(titre);
    trouvé := FALSE;
    FOR i := 1 TO nbLivres DO
        IF livres[i].titre = titre AND livres[i].disponible THEN
            trouvé := TRUE;
            livres[i].disponible := FALSE;
            WriteString("Livre emprunté avec succès")
        END;
    END;
    IF NOT trouvé THEN
        WriteString("Livre non trouvé ou non disponible")
    END;
END EmprunterLivre;

PROCEDURE RendreLivre;
VAR
    titre: STRING[50];
    i: INTEGER;
    trouvé: BOOLEAN;
BEGIN
    WriteString("Entrez le titre du livre à rendre : ");
    ReadString(titre);
    trouvé := FALSE;
    FOR i := 1 TO nbLivres DO
        IF livres[i].titre = titre AND NOT livres[i].disponible THEN
            trouvé := TRUE;
            livres[i].disponible := TRUE;
            WriteString("Livre rendu avec succès")
        END;
    END;
    IF NOT trouvé THEN
        WriteString("Livre non trouvé ou déjà disponible")
    END;
END RendreLivre;

PROCEDURE AfficherLivres;
VAR
    i: INTEGER;
BEGIN
    WriteString("Liste des livres : ");
    WriteLn();
    FOR i := 1 TO nbLivres DO
        WriteString(livres[i].titre);
        WriteChar(' ');
        WriteString(livres[i].auteur);
        WriteChar(' ');
        WriteCard(livres[i].année, 0);
        WriteChar(' ');
        IF livres[i].disponible THEN
            WriteString("Disponible")
        ELSE
            WriteString("Emprunté")
        END;
        WriteLn();
    END;
END AfficherLivres;

BEGIN
    nbLivres := 0;
    WriteString("Bienvenue dans le module de gestion de bibliothèque !");
    WriteLn();
    REPEAT
        WriteString("Choisissez une option : ");
        WriteLn();
        WriteString("1. Ajouter un livre");
        WriteLn();
        WriteString("2. Rechercher un livre");
        WriteLn();
        WriteString("3. Emprunter un livre");
        WriteLn();
        WriteString("4. Rendre un livre");
        WriteLn();
        WriteString("5. Afficher la liste des livres");
        WriteLn();
        WriteString("0. Quitter");
        WriteLn();
        VAR choix: CARDINAL;
        ReadCard(choix);
        CASE choix OF
            1: AjouterLivre;
            2: RechercherLivre;
            3: EmprunterLivre;
            4: RendreLivre;
            5: AfficherLivres;
            0:
        END;
    UNTIL choix = 0;
    WriteString("Au revoir !");
    WriteLn();
END Bibliothèque.
```

**Explication du Code**

Ce code complexe en MODULA-2 gère une bibliothèque de livres de manière complète. Il permet les opérations suivantes :

* Ajouter un livre à la bibliothèque
* Rechercher un livre par son titre
* Emprunter un livre (le rendant indisponible)
* Rendre un livre (le rendant disponible)
* Afficher la liste de tous les livres disponibles

Le code utilise un tableau d'enregistrements pour stocker les informations sur les livres, y compris leur titre, auteur, année de publication et disponibilité. Les procédures sont utilisées pour effectuer chaque opération, telles que l'ajout, la recherche, l'emprunt et le retour de livres.

Une variable `nbLivres` suit le nombre de livres actuellement stockés dans la bibliothèque. Le programme principal présente à l'utilisateur un menu d'options et attend une entrée. Selon l'option choisie, la procédure correspondante est exécutée.

En utilisant des structures de données appropriées, des procédures modulaires et des boucles, ce code permet une gestion efficace d'une bibliothèque de livres sur un ordinateur.