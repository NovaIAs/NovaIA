**Système de gestion de bibliothèque en LUA**

Ce code gère une bibliothèque avec des livres, des auteurs et des emprunts. Il utilise des tables pour stocker les données et des fonctions pour manipuler ces données.

```lua
-- Bibliothèque contenant les livres
livres = {}

-- Bibliothèque contenant les auteurs
auteurs = {}

-- Bibliothèque contenant les emprunts
emprunts = {}

-- Fonction pour ajouter un livre
function ajouterLivre(titre, auteur, isbn)
    -- Vérifier si le livre existe déjà
    if livres[isbn] then
        print("Le livre existe déjà")
        return
    end

    -- Créer une nouvelle entrée pour le livre
    livres[isbn] = {titre = titre, auteur = auteur}

    -- Ajouter l'auteur à la liste des auteurs
    if not auteurs[auteur] then
        auteurs[auteur] = {}
    end
    auteurs[auteur][isbn] = true

    print("Livre ajouté")
end

-- Fonction pour emprunter un livre
function emprunterLivre(isbn, emprunteur)
    -- Vérifier si le livre existe
    if not livres[isbn] then
        print("Le livre n'existe pas")
        return
    end

    -- Vérifier si le livre est disponible
    if livres[isbn].disponible == false then
        print("Le livre est déjà emprunté")
        return
    end

    -- Emprunter le livre
    livres[isbn].disponible = false
    emprunts[isbn] = emprunteur

    print("Livre emprunté")
end

-- Fonction pour rendre un livre
function rendreLivre(isbn)
    -- Vérifier si le livre existe
    if not livres[isbn] then
        print("Le livre n'existe pas")
        return
    end

    -- Rendre le livre
    livres[isbn].disponible = true
    emprunts[isbn] = nil

    print("Livre rendu")
end

-- Fonction pour chercher un livre par titre
function chercherLivreTitre(titre)
    -- Itérer sur les livres
    for isbn, livre in pairs(livres) do
        -- Si le titre correspond, afficher les informations du livre
        if livre.titre == titre then
            print("ISBN : ", isbn)
            print("Titre : ", livre.titre)
            print("Auteur : ", livre.auteur)
            print("Disponible : ", livre.disponible)
        end
    end
end

-- Fonction pour chercher un livre par auteur
function chercherLivreAuteur(auteur)
    -- Itérer sur les auteurs
    for isbn, livres_auteur in pairs(auteurs[auteur]) do
        -- Itérer sur les livres de l'auteur
        for isbn_livre, _ in pairs(livres_auteur) do
            -- Afficher les informations du livre
            print("ISBN : ", isbn_livre)
            print("Titre : ", livres[isbn_livre].titre)
            print("Auteur : ", livres[isbn_livre].auteur)
            print("Disponible : ", livres[isbn_livre].disponible)
        end
    end
end

-- Fonction pour afficher tous les livres
function afficherLivres()
    -- Itérer sur les livres
    for isbn, livre in pairs(livres) do
        -- Afficher les informations du livre
        print("ISBN : ", isbn)
        print("Titre : ", livre.titre)
        print("Auteur : ", livre.auteur)
        print("Disponible : ", livre.disponible)
        print()
    end
end

-- Fonction pour afficher tous les auteurs
function afficherAuteurs()
    -- Itérer sur les auteurs
    for auteur, livres_auteur in pairs(auteurs) do
        -- Afficher les informations de l'auteur
        print("Auteur : ", auteur)

        -- Itérer sur les livres de l'auteur
        for isbn, _ in pairs(livres_auteur) do
            -- Afficher le titre du livre
            print(" - ", livres[isbn].titre)
        end

        print()
    end
end

-- Fonction pour afficher tous les emprunts
function afficherEmprunts()
    -- Itérer sur les emprunts
    for isbn, emprunteur in pairs(emprunts) do
        -- Afficher les informations de l'emprunt
        print("ISBN : ", isbn)
        print("Emprunteur : ", emprunteur)
        print()
    end
end

-- Menu principal
while true do
    -- Afficher les options du menu
    print("1. Ajouter un livre")
    print("2. Emprunter un livre")
    print("3. Rendre un livre")
    print("4. Chercher un livre par titre")
    print("5. Chercher un livre par auteur")
    print("6. Afficher tous les livres")
    print("7. Afficher tous les auteurs")
    print("8. Afficher tous les emprunts")
    print("9. Quitter")

    -- Lire la sélection de l'utilisateur
    selection = io.read()

    -- Appeler la fonction correspondante en fonction de la sélection
    case selection do
        1 then ajouterLivre()
        2 then emprunterLivre()
        3 then rendreLivre()
        4 then chercherLivreTitre()
        5 then chercherLivreAuteur()
        6 then afficherLivres()
        7 then afficherAuteurs()
        8 then afficherEmprunts()
        9 then break
        default then print("Sélection invalide")
    end
end
```

**Explication du code :**

* Le code utilise des tables pour stocker les livres, les auteurs et les emprunts.
* Les fonctions sont utilisées pour manipuler les données dans les tables, comme ajouter des livres, emprunter et rendre des livres, et chercher des livres.
* Le menu principal permet à l'utilisateur d'interagir avec le système en sélectionnant différentes options.
* Le code gère les erreurs et les sélections invalides pour offrir une expérience utilisateur plus conviviale.
* Le code utilise des fonctions d'entrée/sortie pour lire la sélection de l'utilisateur et imprimer des informations.