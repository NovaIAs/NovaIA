**Programme : Gestionnaire de base de données avancé**

**But :** Ce programme gère une base de données complexe avec des tables hiérarchisées, des contraintes d'intégrité référentielle et des mécanismes de recherche sophistiqués.

**Code :**

```perl
# Importer les modules nécessaires
use strict;
use warnings;
use DBI;
use DBD::Pg;

# Établir la connexion à la base de données
my $dbh = DBI->connect(
    "DBI:Pg:dbname=ma_bdd;host=localhost;port=5432",
    'mon_utilisateur',
    'mon_mot_de_passe'
);

# Définir la structure de la base de données
$dbh->do(<<'SQL');
CREATE TABLE personnes (
  id_personne SERIAL PRIMARY KEY,
  nom VARCHAR(255) NOT NULL,
  prénom VARCHAR(255) NOT NULL
);

CREATE TABLE adresses (
  id_adresse SERIAL PRIMARY KEY,
  rue VARCHAR(255),
  ville VARCHAR(255),
  code_postal VARCHAR(5),
  id_personne INTEGER REFERENCES personnes(id_personne)
);

CREATE TABLE téléphones (
  id_telephone SERIAL PRIMARY KEY,
  numero VARCHAR(20),
  type VARCHAR(20),
  id_personne INTEGER REFERENCES personnes(id_personne)
);
SQL

# Requête de recherche complexe avec jointures
my $requete = $dbh->prepare(<<'SQL');
SELECT *
FROM personnes
LEFT JOIN adresses ON personnes.id_personne = adresses.id_personne
LEFT JOIN téléphones ON personnes.id_personne = téléphones.id_personne
WHERE personnes.nom LIKE '%mon_critère%'
AND téléphones.type = 'mobile';
SQL

# Exécuter la requête et récupérer les résultats
my $sth = $requete->execute;
while (my $ligne = $sth->fetchrow_hashref) {
  print "Personne : $ligne->{nom} $ligne->{prénom}\n";
  if ($ligne->{rue}) {
    print "Adresse : $ligne->{rue}, $ligne->{ville}, $ligne->{code_postal}\n";
  }
  if ($ligne->{numero}) {
    print "Téléphone : $ligne->{numero}\n";
  }
}

# Fermer la connexion à la base de données
$dbh->disconnect;
```

**Explications :**

* Le code établit une connexion à une base de données PostgreSQL via le module DBI.
* Il définit la structure de la base de données avec trois tables liées : personnes, adresses et téléphones.
* Une requête complexe est préparée, utilisant des jointures pour récupérer les données de plusieurs tables en fonction de critères de recherche.
* La requête est exécutée et les résultats sont récupérés et affichés.
* Enfin, la connexion à la base de données est fermée.

Ce code offre des fonctionnalités avancées de gestion de base de données, notamment :

* Gestion des tables hiérarchisées
* Contraintes d'intégrité référentielle
* Requêtes complexes avec jointures
* Recherche sophistiquée avec des critères multiples