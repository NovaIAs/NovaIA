```php
<?php

// Nom du fichier de configuration
$fichier_config = 'config.ini';

// Charge le fichier de configuration
$config = parse_ini_file($fichier_config);

// Vérifie si la configuration est valide
if (!$config) {
    throw new Exception('Fichier de configuration invalide');
}

// Récupère les paramètres de configuration
$base_url = $config['base_url'];
$nom_base = $config['nom_base'];
$utilisateur = $config['utilisateur'];
$mdp = $config['mdp'];

// Se connecte à la base de données
$bdd = new PDO("mysql:host=$base_url;dbname=$nom_base", $utilisateur, $mdp);

// Prépare une requête SQL
$requete = $bdd->prepare("SELECT * FROM utilisateurs WHERE nom = ?");

// Exécute la requête avec un paramètre
$requete->execute(array('Dupont'));

// Récupère le résultat de la requête
$resultats = $requete->fetchAll(PDO::FETCH_ASSOC);

// Affiche les résultats
foreach ($resultats as $utilisateur) {
    echo "Nom : " . $utilisateur['nom'] . "<br>";
    echo "Prénom : " . $utilisateur['prenom'] . "<br>";
    echo "Email : " . $utilisateur['email'] . "<br><br>";
}

// Prépare une requête SQL avec une jointure
$requete = $bdd->prepare("SELECT u.nom AS nom_utilisateur, p.nom AS nom_projet 
                           FROM utilisateurs u 
                           JOIN projets p ON u.id = p.id_utilisateur
                           WHERE p.id = ?");

// Exécute la requête avec un paramètre
$requete->execute(array(1));

// Récupère le résultat de la requête
$resultats = $requete->fetchAll(PDO::FETCH_ASSOC);

// Affiche les résultats
foreach ($resultats as $utilisateur) {
    echo "Nom utilisateur : " . $utilisateur['nom_utilisateur'] . "<br>";
    echo "Nom projet : " . $utilisateur['nom_projet'] . "<br><br>";
}

// Prépare une requête SQL avec une clause IN
$requete = $bdd->prepare("SELECT * FROM utilisateurs WHERE id IN (?)");

// Exécute la requête avec un tableau de paramètres
$requete->execute(array(array(1, 2, 3)));

// Récupère le résultat de la requête
$resultats = $requete->fetchAll(PDO::FETCH_ASSOC);

// Affiche les résultats
foreach ($resultats as $utilisateur) {
    echo "Nom : " . $utilisateur['nom'] . "<br>";
    echo "Prénom : " . $utilisateur['prenom'] . "<br>";
    echo "Email : " . $utilisateur['email'] . "<br><br>";
}

// Prépare une requête SQL avec une clause LIKE
$requete = $bdd->prepare("SELECT * FROM utilisateurs WHERE nom LIKE ?");

// Exécute la requête avec un paramètre
$requete->execute(array('D%'));

// Récupère le résultat de la requête
$resultats = $requete->fetchAll(PDO::FETCH_ASSOC);

// Affiche les résultats
foreach ($resultats as $utilisateur) {
    echo "Nom : " . $utilisateur['nom'] . "<br>";
    echo "Prénom : " . $utilisateur['prenom'] . "<br>";
    echo "Email : " . $utilisateur['email'] . "<br><br>";
}

// Prépare une requête SQL avec une fonction SQL
$requete = $bdd->prepare("SELECT COUNT(*) AS nombre_utilisateurs FROM utilisateurs");

// Exécute la requête
$requete->execute();

// Récupère le résultat de la requête
$resultats = $requete->fetch(PDO::FETCH_ASSOC);

// Affiche le résultat
echo "Nombre d'utilisateurs : " . $resultats['nombre_utilisateurs'];
?>
```

**Explication du code :**

Ce code PHP se connecte à une base de données MySQL et exécute différentes requêtes SQL complexes en utilisant des paramètres et des clauses avancées.

1. **Chargement de la configuration de la base de données :** Le code charge les paramètres de configuration de la base de données à partir d'un fichier de configuration.
2. **Connexion à la base de données :** Une connexion PDO est établie à la base de données en utilisant les paramètres de configuration.
3. **Requête SQL avec paramètre :** Une requête SQL est préparée avec un paramètre pour récupérer des données spécifiques d'une table.
4. **Requête SQL avec jointure :** Une requête SQL avec une jointure est préparée pour récupérer des données de deux tables liées.
5. **Requête SQL avec clause IN :** Une requête SQL avec une clause IN est préparée pour récupérer des données d'une table en fonction d'une liste de valeurs.
6. **Requête SQL avec clause LIKE :** Une requête SQL avec une clause LIKE est préparée pour récupérer des données d'une table en fonction d'un critère de correspondance partielle.
7. **Requête SQL avec fonction SQL :** Une requête SQL avec une fonction SQL est préparée pour récupérer des données agrégées à partir d'une table.

Ce code montre des techniques de programmation PHP avancées pour l'interaction avec une base de données, notamment l'utilisation de requêtes préparées avec des paramètres, des clauses avancées et des fonctions SQL.