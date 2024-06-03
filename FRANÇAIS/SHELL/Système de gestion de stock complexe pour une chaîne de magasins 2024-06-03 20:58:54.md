**Objectif du code:** Gérer un système de gestion de stock complexe pour une chaîne de magasins.

**Explication du code:**

```shell
#!/bin/bash

# Définir les variables
base_de_données="stock.db"
articles="articles.txt"
commandes="commandes.txt"
fournisseurs="fournisseurs.txt"

# Fonctions pour gérer la base de données
creer_base_de_données() {
  sqlite3 "$base_de_données" < schema.sql
}

ajouter_article() {
  local id=$1
  local nom=$2
  local prix=$3
  sqlite3 "$base_de_données" "INSERT INTO articles (id, nom, prix) VALUES ($id, '$nom', $prix);"
}

get_article() {
  local id=$1
  sqlite3 "$base_de_données" "SELECT * FROM articles WHERE id=$id;"
}

supprimer_article() {
  local id=$1
  sqlite3 "$base_de_données" "DELETE FROM articles WHERE id=$id;"
}

# Fonctions pour gérer les articles
lire_articles() {
  cat "$articles"
}

ajouter_article_fichier() {
  local id=$1
  local nom=$2
  local prix=$3
  echo "$id $nom $prix" >> "$articles"
}

# Fonctions pour gérer les commandes
lire_commandes() {
  cat "$commandes"
}

ajouter_commande() {
  local id=$1
  local article_id=$2
  local quantité=$3
  echo "$id $article_id $quantité" >> "$commandes"
}

# Fonctions pour gérer les fournisseurs
lire_fournisseurs() {
  cat "$fournisseurs"
}

ajouter_fournisseur() {
  local nom=$1
  local adresse=$2
  echo "$nom $adresse" >> "$fournisseurs"
}

# Menu principal
while true; do
  clear
  echo "Système de gestion de stock"
  echo "========================"
  echo "1. Gérer la base de données"
  echo "2. Gérer les articles"
  echo "3. Gérer les commandes"
  echo "4. Gérer les fournisseurs"
  echo "5. Quitter"
  echo "Choix : "

  read choix

  case $choix in
    1)
      while true; do
        clear
        echo "Gestion de la base de données"
        echo "=========================="
        echo "1. Créer une base de données"
        echo "2. Ajouter un article"
        echo "3. Obtenir un article"
        echo "4. Supprimer un article"
        echo "5. Revenir au menu principal"
        echo "Choix : "

        read sous_choix

        case $sous_choix in
          1)
            creer_base_de_données
            echo "Base de données créée avec succès !"
            sleep 2
            ;;
          2)
            echo "Entrez l'ID de l'article : "
            read id
            echo "Entrez le nom de l'article : "
            read nom
            echo "Entrez le prix de l'article : "
            read prix
            ajouter_article $id "$nom" $prix
            echo "Article ajouté avec succès !"
            sleep 2
            ;;
          3)
            echo "Entrez l'ID de l'article : "
            read id
            article=$(get_article $id)
            if [[ ! -z "$article" ]]; then
              echo "Article trouvé :"
              echo "$article"
            else
              echo "Aucun article trouvé avec cet ID."
            fi
            sleep 2
            ;;
          4)
            echo "Entrez l'ID de l'article : "
            read id
            supprimer_article $id
            echo "Article supprimé avec succès !"
            sleep 2
            ;;
          5)
            break
            ;;
          *)
            echo "Choix invalide !"
            sleep 2
            ;;
        esac
      done
      ;;
    2)
      while true; do
        clear
        echo "Gestion des articles"
        echo "==================="
        echo "1. Lire les articles"
        echo "2. Ajouter un article"
        echo "3. Revenir au menu principal"
        echo "Choix : "

        read sous_choix

        case $sous_choix in
          1)
            articles=$(lire_articles)
            if [[ ! -z "$articles" ]]; then
              echo "Liste des articles :"
              echo "$articles"
            else
              echo "Aucun article trouvé."
            fi
            sleep 2
            ;;
          2)
            echo "Entrez l'ID de l'article : "
            read id
            echo "Entrez le nom de l'article : "
            read nom
            echo "Entrez le prix de l'article : "
            read prix
            ajouter_article_fichier $id "$nom" $prix
            echo "Article ajouté avec succès !"
            sleep 2
            ;;
          3)
            break
            ;;
          *)
            echo "Choix invalide !"
            sleep 2
            ;;
        esac
      done
      ;;
    3)
      while true; do
        clear
        echo "Gestion des commandes"
        echo "==================="
        echo "1. Lire les commandes"
        echo "2. Ajouter une commande"
        echo "3. Revenir au menu principal"
        echo "Choix : "

        read sous_choix

        case $sous_choix in
          1)
            commandes=$(lire_commandes)
            if [[ ! -z "$commandes" ]]; then
              echo "Liste des commandes :"
              echo "$commandes"
            else
              echo "Aucune commande trouvée."
            fi
            sleep 2
            ;;
          2)
            echo "Entrez l'ID de la commande : "
            read id
            echo "Entrez l'ID de l'article : "
            read article_id
            echo "Entrez la quantité : "
            read quantité
            ajouter_commande $id $article_id $quantité
            echo "Commande ajoutée avec succès !"
            sleep 2
            ;;
          3)
            break
            ;;
          *)
            echo "Choix invalide !"
            sleep 2
            ;;
        esac
      done
      ;;
    4)
      while true; do
        clear
        echo "Gestion des fournisseurs"
        echo "======================"
        echo "1. Lire les fournisseurs"
        echo "2. Ajouter un fournisseur"
        echo "3. Revenir au menu principal"
        echo "Choix : "

        read sous_choix

        case $sous_choix in
          1)
            fournisseurs=$(lire_fournisseurs)
            if [[ ! -z "$fournisseurs" ]]; then
              echo "Liste des fournisseurs :"
              echo "$fournisseurs"
            else
              echo "Aucun fournisseur trouvé."
            fi
            sleep 2
            ;;
          2)
            echo "Entrez le nom du fournisseur : "
            read nom
            echo "Entrez l'adresse du fournisseur : "
            read adresse
            ajouter_fournisseur $nom "$adresse"
            echo "Fournisseur ajouté avec succès !"
            sleep 2
            ;;
          3)
            break
            ;;
          *)
            echo "Choix invalide !"
            sleep 2
            ;;
        esac
      done
      ;;
    5)
      echo "Au revoir !"
      exit 0
      ;;
  esac
done
```