**Gestionnaire de fichiers avancé en SHELL**

```shell
#!/bin/bash

# Fonctions auxiliaires

function liste_fichiers() {
  find "$1" -type f \( -iname '*txt' -o -iname '*sh' \) -exec echo {} \;
}

function compte_lignes() {
  wc -l "$1" | cut -d' ' -f1
}

function affiche_histogramme() {
  nl < "$1" | awk '{print NR "\t" $1}' | tr -d '\n' | sed 's/\t/\t|\t/g'
}

# Menu principal

while true; do
  echo "Gestionnaire de fichiers"
  echo "----------------------"
  echo "1. Lister les fichiers"
  echo "2. Compter les lignes d'un fichier"
  echo "3. Afficher l'histogramme de fréquence des mots"
  echo "4. Quitter"
  echo -n "Votre choix (1/2/3/4) : "
  read choix

  # Traitement du choix

  case "$choix" in
    "1")
      echo "Entrez le répertoire : "
      read repertoire
      liste_fichiers "$repertoire"
      ;;
    "2")
      echo "Entrez le fichier : "
      read fichier
      compte_lignes "$fichier"
      ;;
    "3")
      echo "Entrez le fichier : "
      read fichier
      affiche_histogramme "$fichier"
      ;;
    "4")
      echo "Au revoir !"
      exit 0
      ;;
    *)
      echo "Choix invalide"
      ;;
  esac
done
```

**Explication du code :**

* Ce script gère les fichiers de manière avancée, offrant trois fonctionnalités : le listing des fichiers, le comptage des lignes d'un fichier et l'affichage de l'histogramme de fréquence des mots.
* Il utilise des fonctions auxiliaires pour encapsuler des tâches spécifiques.
* Le menu principal présente les options de l'utilisateur et traite son choix grâce à la structure de contrôle `case`.
* La fonction `liste_fichiers` utilise `find` pour répertorier les fichiers d'un répertoire donné correspondant à certains critères.
* La fonction `compte_lignes` utilise `wc` pour compter les lignes d'un fichier.
* La fonction `affiche_histogramme` utilise `nl`, `awk`, `tr` et `sed` pour créer un histogramme de fréquence des mots.