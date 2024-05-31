**Script de Gestion de Parc Informatique en Français**

```shell
#!/bin/bash

# Définir les variables d'environnement
export PATH=/usr/local/bin:/usr/bin:/bin
export LANG=fr_FR.UTF-8

# Fonctions
function afficherMenu() {
  echo "Menu de gestion du parc informatique :"
  echo "1 - Lister les ordinateurs"
  echo "2 - Ajouter un ordinateur"
  echo "3 - Modifier un ordinateur"
  echo "4 - Supprimer un ordinateur"
  echo "5 - Quitter"
  read -p "Votre choix : " choix
}

function listerOrdinateurs() {
  echo "Liste des ordinateurs :"
  # Afficher la liste des ordinateurs stockés dans le fichier "ordinateurs.txt"
  cat ordinateurs.txt
}

function ajouterOrdinateur() {
  echo "Ajouter un ordinateur :"
  # Saisir le nom, le système d'exploitation et l'adresse IP de l'ordinateur
  read -p "Nom : " nom
  read -p "Système d'exploitation : " os
  read -p "Adresse IP : " ip
  # Ajouter l'ordinateur au fichier "ordinateurs.txt"
  echo "$nom;$os;$ip" >> ordinateurs.txt
  echo "Ordinateur ajouté."
}

function modifierOrdinateur() {
  echo "Modifier un ordinateur :"
  # Saisir le nom de l'ordinateur à modifier
  read -p "Nom de l'ordinateur à modifier : " nom
  # Récupérer les informations de l'ordinateur à modifier
  ordinateur=$(grep "$nom" ordinateurs.txt)
  # Séparer les informations de l'ordinateur par des points-virgules
  IFS=';' read -r nom os ip <<< "$ordinateur"
  # Modifier les informations de l'ordinateur
  read -p "Nouveau nom : " newNom
  read -p "Nouveau système d'exploitation : " newOs
  read -p "Nouvelle adresse IP : " newIp
  # Remplacer les anciennes informations par les nouvelles dans le fichier "ordinateurs.txt"
  sed -i "s/$nom;$os;$ip/$newNom;$newOs;$newIp/g" ordinateurs.txt
  echo "Ordinateur modifié."
}

function supprimerOrdinateur() {
  echo "Supprimer un ordinateur :"
  # Saisir le nom de l'ordinateur à supprimer
  read -p "Nom de l'ordinateur à supprimer : " nom
  # Supprimer l'ordinateur du fichier "ordinateurs.txt"
  sed -i "/$nom/d" ordinateurs.txt
  echo "Ordinateur supprimé."
}

# Boucle principale
while true; do
  afficherMenu
  case $choix in
    1)
      listerOrdinateurs
      ;;
    2)
      ajouterOrdinateur
      ;;
    3)
      modifierOrdinateur
      ;;
    4)
      supprimerOrdinateur
      ;;
    5)
      echo "Au revoir !"
      exit 0
      ;;
    *)
      echo "Choix invalide."
      ;;
  esac
done
```

**Explication du code :**

Ce script bash gère un parc informatique en français. Il permet de lister, ajouter, modifier et supprimer des ordinateurs.

* Les fonctions :
  * `afficherMenu` affiche le menu principal.
  * `listerOrdinateurs` liste les ordinateurs stockés dans le fichier "ordinateurs.txt".
  * `ajouterOrdinateur` ajoute un ordinateur au fichier "ordinateurs.txt".
  * `modifierOrdinateur` modifie les informations d'un ordinateur dans le fichier "ordinateurs.txt".
  * `supprimerOrdinateur` supprime un ordinateur du fichier "ordinateurs.txt".
* La boucle principale :
  * Elle affiche le menu principal et demande à l'utilisateur de faire un choix.
  * En fonction du choix de l'utilisateur, elle appelle la fonction correspondante.
  * L'utilisateur peut quitter le script en choisissant l'option "5".