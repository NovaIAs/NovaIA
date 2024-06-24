**Diagramme de classes**

```uml
+----------------+
| Utilisateur     |
+----------------+
   - id : int
   - nom : string
   - prenom : string
   - email : string
   - password : string

+----------------+
| Role            |
+----------------+
   - id : int
   - nom : string
   - description : string

+----------------+
| UtilisateurRole |
+----------------+
   - utilisateurId : int
   - roleId : int

+----------------+
| Film            |
+----------------+
   - id : int
   - titre : string
   - annee : int
   - genre : string
   - duree : int
   - resume : string

+----------------+
| Categorie       |
+----------------+
   - id : int
   - nom : string
   - description : string

+----------------+
| FilmCategorie   |
+----------------+
   - filmId : int
   - categorieId : int

+----------------+
| Acteur          |
+----------------+
   - id : int
   - nom : string
   - prenom : string
   - dateNaissance : date

+----------------+
| FilmActeur      |
+----------------+
   - filmId : int
   - acteurId : int

+----------------+
| Realisateur     |
+----------------+
   - id : int
   - nom : string
   - prenom : string

+----------------+
| FilmRealisateur |
+----------------+
   - filmId : int
   - realisateurId : int

+----------------+
| Avis            |
+----------------+
   - id : int
   - utilisateurId : int
   - filmId : int
   - note : int
   - commentaire : string
```

**Diagramme de séquence**

Le diagramme de séquence décrit l'interaction entre les objets du système au fil du temps. Il montre les objets impliqués, les messages qu'ils s'échangent et l'ordre dans lequel ces messages sont échangés.

```sequence
participant Utilisateur
participant Role
participant Film
participant Categorie
participant Acteur
participant Realisateur
participant Avis

Utilisateur->Role : getRoles()
Role->Utilisateur : roles
Utilisateur->Film : getFilms()
Film->Utilisateur : films
Utilisateur->Categorie : getCategories()
Categorie->Utilisateur : categories
Utilisateur->Acteur : getActeurs()
Acteur->Utilisateur : acteurs
Utilisateur->Realisateur : getRealisateurs()
Realisateur->Utilisateur : realisateurs
Utilisateur->Avis : getAvis()
Avis->Utilisateur : avis
```

**Diagramme de cas d'utilisation**

Le diagramme de cas d'utilisation décrit les fonctionnalités du système du point de vue des utilisateurs. Il montre les acteurs qui interagissent avec le système, les cas d'utilisation qu'ils peuvent exécuter et les relations entre ces cas d'utilisation.

```usecase
acteur Utilisateur
cas d'utilisation S'inscrire
cas d'utilisation Se connecter
cas d'utilisation Se déconnecter
cas d'utilisation Naviguer dans le catalogue
cas d'utilisation Rechercher des films
cas d'utilisation Afficher les détails d'un film
cas d'utilisation Ajouter un film à la liste des favoris
cas d'utilisation Supprimer un film de la liste des favoris
cas d'utilisation Poster un avis
cas d'utilisation Modifier un avis
cas d'utilisation Supprimer un avis
```

**Diagramme de déploiement**

Le diagramme de déploiement décrit l'architecture physique du système. Il montre les nœuds matériels, les logiciels exécutés sur ces nœuds et les dépendances entre ces composants.

```deployment
nœud Serveur d'applications
nœud Base de données
nœud Client Web

Serveur d'applications->Base de données : connexion
Client Web->Serveur d'applications : requête HTTP
Serveur d'applications->Base de données : requête SQL
```