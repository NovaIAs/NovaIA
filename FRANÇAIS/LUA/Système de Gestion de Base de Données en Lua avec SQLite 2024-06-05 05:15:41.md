**Système de Gestion de Base de Données en Lua**

```lua
-- Importer la bibliothèque SQLite
local sqlite3 = require("sqlite3")

-- Ouvrir la base de données "ma_base.sqlite"
local db = sqlite3.open("ma_base.sqlite")

-- Créer une table "utilisateurs"
local query = [[
CREATE TABLE utilisateurs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  nom TEXT,
  email TEXT,
  telephone TEXT
);
]]
db:execute(query)

-- Insérer des données dans la table "utilisateurs"
local query = [[
INSERT INTO utilisateurs (nom, email, telephone)
VALUES
  ('Alice', 'alice@exemple.com', '0612345678'),
  ('Bob', 'bob@exemple.com', '0689101112'),
  ('Charlie', 'charlie@exemple.com', '0623456789');
]]
db:execute(query)

-- Récupérer les utilisateurs dont le nom contient "li"
local query = [[
SELECT * FROM utilisateurs
WHERE nom LIKE '%li%';
]]
local result = db:execute(query)

-- Afficher les résultats
for i = 1, #result do
  print(result[i].id, result[i].nom, result[i].email, result[i].telephone)
end

-- Fermer la connexion à la base de données
db:close()
```

**Explication du Code**

Ce code crée un système de gestion de base de données simple en Lua en utilisant la bibliothèque SQLite. Il crée une table, insère des données dans la table et récupère les données de la table en fonction d'un critère de recherche.

* **Ligne 2 :** Importe la bibliothèque SQLite.
* **Ligne 5 :** Ouvre la base de données "ma_base.sqlite" et renvoie un objet de base de données.
* **Ligne 9 :** Crée une table "utilisateurs" avec des colonnes "id", "nom", "email" et "telephone".
* **Ligne 17 :** Insère trois lignes de données dans la table "utilisateurs".
* **Ligne 26 :** Prépare une requête pour récupérer les utilisateurs dont le nom contient "li".
* **Ligne 29 :** Exécute la requête et renvoie un tableau de résultats.
* **Ligne 33 - 36 :** Itère sur les résultats et affiche l'ID, le nom, l'email et le numéro de téléphone de chaque utilisateur.
* **Ligne 40 :** Ferme la connexion à la base de données.