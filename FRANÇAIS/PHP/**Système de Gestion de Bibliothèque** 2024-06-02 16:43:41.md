**Système de gestion de bibliothèque**

**Fonctions principales**

* Enregistrer les membres
* Enregistrer les livres
* Gérer les prêts et les retours
* Rechercher des livres et des membres
* Générer des rapports

**Code PHP**

```php
<?php

// Connexion à la base de données
$db = mysqli_connect('localhost', 'root', '', 'bibliotheque');

// Fonctions

// Enregistrer un membre
function enregistrerMembre($nom, $prenom, $adresse, $telephone, $email) {
  global $db;

  $sql = "INSERT INTO membres (nom, prenom, adresse, telephone, email) VALUES (?, ?, ?, ?, ?)";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "sssss", $nom, $prenom, $adresse, $telephone, $email);
  mysqli_stmt_execute($stmt);

  mysqli_stmt_close($stmt);
}

// Enregistrer un livre
function enregistrerLivre($titre, $auteur, $annee) {
  global $db;

  $sql = "INSERT INTO livres (titre, auteur, annee) VALUES (?, ?, ?)";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "ssi", $titre, $auteur, $annee);
  mysqli_stmt_execute($stmt);

  mysqli_stmt_close($stmt);
}

// Prêter un livre
function preterLivre($idMembre, $idLivre, $datePret) {
  global $db;

  $sql = "INSERT INTO prets (idMembre, idLivre, datePret) VALUES (?, ?, ?)";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "iii", $idMembre, $idLivre, $datePret);
  mysqli_stmt_execute($stmt);

  mysqli_stmt_close($stmt);
}

// Retourner un livre
function retournerLivre($idMembre, $idLivre, $dateRetour) {
  global $db;

  $sql = "UPDATE prets SET dateRetour = ? WHERE idMembre = ? AND idLivre = ?";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "sii", $dateRetour, $idMembre, $idLivre);
  mysqli_stmt_execute($stmt);

  mysqli_stmt_close($stmt);
}

// Rechercher un livre
function rechercherLivre($titre, $auteur, $annee) {
  global $db;

  $sql = "SELECT * FROM livres WHERE titre LIKE ? AND auteur LIKE ? AND annee LIKE ?";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "sss", $titre, $auteur, $annee);
  mysqli_stmt_execute($stmt);

  $result = mysqli_stmt_get_result($stmt);

  mysqli_stmt_close($stmt);

  return $result;
}

// Rechercher un membre
function rechercherMembre($nom, $prenom, $adresse, $telephone, $email) {
  global $db;

  $sql = "SELECT * FROM membres WHERE nom LIKE ? AND prenom LIKE ? AND adresse LIKE ? AND telephone LIKE ? AND email LIKE ?";

  $stmt = mysqli_prepare($db, $sql);
  mysqli_stmt_bind_param($stmt, "sssss", $nom, $prenom, $adresse, $telephone, $email);
  mysqli_stmt_execute($stmt);

  $result = mysqli_stmt_get_result($stmt);

  mysqli_stmt_close($stmt);

  return $result;
}

// Générer un rapport des prêts en cours
function genererRapportPrets() {
  global $db;

  $sql = "SELECT * FROM prets WHERE dateRetour IS NULL";

  $result = mysqli_query($db, $sql);

  return $result;
}

// Exécution du script

// Enregistrer un nouveau membre
enregistrerMembre('Dupont', 'Jean', '12 rue du Bois', '0123456789', 'jean.dupont@email.com');

// Enregistrer un nouveau livre
enregistrerLivre('Le Petit Prince', 'Antoine de Saint-Exupéry', 1943);

// Prêter un livre
preterLivre(1, 1, '2023-01-01');

// Rechercher un livre
$livres = rechercherLivre('Le Petit Prince', '', '');

// Afficher les résultats de la recherche
foreach ($livres as $livre) {
  echo $livre['titre'] . ' par ' . $livre['auteur'] . ' (' . $livre['annee'] . ')';
}

// Générer un rapport des prêts en cours
$prets = genererRapportPrets();

// Envoyer le rapport par email

?>
```

**Explication du code**

Ce code PHP implémente un système de gestion de bibliothèque simple avec les fonctionnalités suivantes :

* Enregistrement des membres
* Enregistrement des livres
* Gestion des prêts et des retours
* Recherche des livres et des membres
* Génération des rapports

Le code utilise des fonctions pour chaque tâche, ce qui le rend facile à lire et à maintenir.

La connexion à la base de données est établie à l'aide de `mysqli_connect()`.

Les fonctions d'enregistrement (`enregistrerMembre()` et `enregistrerLivre()`) utilisent des requêtes préparées pour empêcher les injections SQL.

La fonction `preterLivre()` insère un nouveau prêt dans la table `prets`, tandis que la fonction `retournerLivre()` met à jour la date de retour.

Les fonctions de recherche (`rechercherLivre()` et `rechercherMembre()`) utilisent des requêtes SQL avec des caractères génériques (%) pour rechercher des correspondances partielles.

La fonction `genererRapportPrets()` génère un rapport des prêts en cours en sélectionnant tous les prêts où la date de retour est `NULL`.

Enfin, le script exécute différentes tâches, telles que l'enregistrement d'un membre, l'enregistrement d'un livre, le prêt d'un livre, la recherche de livres et la génération d'un rapport des prêts en cours.