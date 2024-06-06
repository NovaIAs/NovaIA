**Système de gestion de bibliothèque en Ruby**

Ce système de gestion de bibliothèque permet aux bibliothécaires de gérer les livres, les emprunts et les adhérents. Il fournit une interface conviviale pour ajouter, rechercher et mettre à jour les données.

**Structure du code**

Le système est organisé en plusieurs modules et classes :

* **Bibliothécaire** : Représente un bibliothécaire avec des informations d'identification et des autorisations.
* **Adhérent** : Représente un adhérent à la bibliothèque avec des informations personnelles et un historique d'emprunt.
* **Livre** : Représente un livre avec des informations bibliographiques, des exemplaires et un statut.
* **Exemplaire** : Représente un exemplaire d'un livre avec une disponibilité et un emplacement.
* **Emprunt** : Représente un emprunt avec des informations sur l'adhérent, le livre et les dates d'emprunt et de retour.
* **Application** : Point d'entrée de l'application qui gère les interactions avec l'utilisateur et les objets du système.

**Interface utilisateur**

Le système utilise l'interface de ligne de commande (CLI) pour interagir avec l'utilisateur. La CLI fournit des menus et des options pour effectuer diverses opérations :

```ruby
# Affichage du menu principal
puts "Menu principal"
puts "1. Gestion des adhérents"
puts "2. Gestion des livres"
puts "3. Gestion des emprunts"

# Récupération du choix de l'utilisateur
choix = gets.chomp.to_i
```

**Gestion des adhérents**

Les bibliothécaires peuvent ajouter, rechercher, mettre à jour et supprimer des adhérents :

```ruby
# Ajout d'un adhérent
adherent = Adherent.new(nom: "Dupont", prenom: "Jean")
adherent.save

# Recherche d'un adhérent
adherent = Adherent.find_by(nom: "Dupont")

# Mise à jour d'un adhérent
adherent.email = "jean.dupont@email.com"
adherent.save

# Suppression d'un adhérent
adherent.destroy
```

**Gestion des livres**

Les bibliothécaires peuvent ajouter, rechercher, mettre à jour et supprimer des livres :

```ruby
# Ajout d'un livre
livre = Livre.new(titre: "Les Misérables", auteur: "Victor Hugo")
livre.save

# Recherche d'un livre
livre = Livre.find_by(titre: "Les Misérables")

# Mise à jour d'un livre
livre.edition = "édition spéciale"
livre.save

# Suppression d'un livre
livre.destroy
```

**Gestion des emprunts**

Les bibliothécaires peuvent enregistrer, rechercher et retourner des emprunts :

```ruby
# Enregistrement d'un emprunt
emprunt = Emprunt.new(adherent_id: 1, livre_id: 1, date_emprunt: Date.today)
emprunt.save

# Recherche d'un emprunt
emprunt = Emprunt.find_by(adherent_id: 1, livre_id: 1)

# Retour d'un emprunt
emprunt.date_retour = Date.today
emprunt.save
```

**Sécurité**

Le système utilise un mécanisme d'authentification de base pour sécuriser l'accès :

```ruby
# Authentification du bibliothécaire
biblio = Bibliothécaire.find_by(nom: "admin", mot_de_passe: "secret")
if biblio
  # Bibliothécaire authentifié, accès autorisé
else
  # Bibliothécaire non authentifié, accès refusé
end
```

**Conclusion**

Ce système de gestion de bibliothèque en Ruby est un exemple complexe de programmation orientée objet qui démontre la gestion des données, les opérations CRUD (création, lecture, mise à jour, suppression) et la sécurité. Il peut être adapté pour répondre à des besoins spécifiques et fournir une solution fiable pour la gestion des bibliothèques.