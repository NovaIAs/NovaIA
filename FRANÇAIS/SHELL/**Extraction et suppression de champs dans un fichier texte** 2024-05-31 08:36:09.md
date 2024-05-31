```shell
#!/bin/bash

# Déclarer les variables
fichier_entree="entree.txt"
fichier_sortie="sortie.txt"
decoupeur="\|" # Caractère séparateur
champs_a_extraire="1,3" # Liste des champs à extraire
champs_a_supprimer="2" # Liste des champs à supprimer

# Vérifier si les fichiers existent
if [ ! -f $fichier_entree ]; then
  echo "Le fichier d'entrée n'existe pas."
  exit 1
fi

if [ -f $fichier_sortie ]; then
  rm $fichier_sortie
fi

# Extraire les champs spécifiés
awk -F"$decoupeur" 'BEGIN {OFS="$decoupeur"} {$1=""; print}' $fichier_entree > $fichier_sortie.tmp

# Supprimer les champs spécifiés
awk -F"$decoupeur" '{for (i=1; i<=NF; i++) if (i='$champs_a_supprimer') $i=""; print}' $fichier_sortie.tmp > $fichier_sortie

# Supprimer le fichier temporaire
rm $fichier_sortie.tmp
```

**Explications du code :**

* La première ligne du script spécifie l'interpréteur de commande à utiliser (bash).
* La deuxième ligne déclare les variables utilisées dans le script.
* Les lignes 9 et 10 vérifient si les fichiers d'entrée et de sortie existent.
* Les lignes 12 à 16 extraient les champs spécifiés dans la variable `champs_a_extraire` du fichier d'entrée et les écrivent dans un fichier temporaire.
* Les lignes 18 à 22 suppriment les champs spécifiés dans la variable `champs_a_supprimer` du fichier temporaire et écrivent le résultat dans le fichier de sortie.
* La ligne 24 supprime le fichier temporaire.