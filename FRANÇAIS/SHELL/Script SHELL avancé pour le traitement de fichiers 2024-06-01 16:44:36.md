**Code SHELL complexe**

```shell
#!/bin/bash

# Déclaration des variables
fichier_entre=$(mktemp -t entre.XXXXXX)
fichier_sortie=$(mktemp -t sortie.XXXXXX)

# Vérification des arguments
if [ $# -lt 3 ]; then
    echo "Utilisation : $0 <fichier_entrée> <fichier_sortie> <opération>"
    exit 1
fi

# Lecture du fichier d'entrée
cat $1 | while read ligne; do
    # Traitement de la ligne en fonction de l'opération spécifiée
    case $3 in
        "additionner")
            # Additionner la valeur de la ligne à une variable
            somme=$(($somme + $ligne))
            ;;
        "trier")
            # Ajouter la ligne à un fichier intermédiaire pour le tri
            echo $ligne >> $fichier_entre
            ;;
        "compter")
            # Compter le nombre d'occurrences de la ligne
            compteur=$(($compteur + 1))
            ;;
        *)
            echo "Opération $3 non reconnue"
            exit 1
            ;;
    esac
done

# Traitement post-opération
case $3 in
    "additionner")
        echo "Somme des valeurs : $somme" >> $fichier_sortie
        ;;
    "trier")
        # Trier le fichier intermédiaire et l'écrire dans le fichier de sortie
        sort $fichier_entre > $fichier_sortie
        ;;
    "compter")
        echo "Nombre d'occurrences : $compteur" >> $fichier_sortie
        ;;
esac

# Suppression des fichiers temporaires
rm $fichier_entre $fichier_sortie
```

**Explication du code**

Ce code est un script SHELL qui effectue diverses opérations sur un fichier d'entrée et écrit les résultats dans un fichier de sortie. L'opération à effectuer est spécifiée par le troisième argument.

**Déclaration des variables**

* `fichier_entre`: Fichier temporaire utilisé pour stocker les lignes intermédiaires.
* `fichier_sortie`: Fichier temporaire utilisé pour stocker les résultats.

**Vérification des arguments**

Le code vérifie que trois arguments ont été fournis : le fichier d'entrée, le fichier de sortie et l'opération.

**Lecture du fichier d'entrée**

Le fichier d'entrée est lu ligne par ligne.

**Traitement de chaque ligne**

En fonction de l'opération spécifiée, le script effectue l'opération appropriée sur la ligne :

* **Additionner:** Additionne la valeur de la ligne à une variable.
* **Trier:** Ajoute la ligne à un fichier intermédiaire pour le tri.
* **Compter:** Incrémente un compteur pour chaque occurrence de la ligne.

**Traitement post-opération**

Une fois que toutes les lignes ont été traitées, le script effectue l'opération post-opération appropriée :

* **Additionner:** Écrit la somme des valeurs dans le fichier de sortie.
* **Trier:** Trie le fichier intermédiaire et l'écrit dans le fichier de sortie.
* **Compter:** Écrit le nombre d'occurrences dans le fichier de sortie.

**Suppression des fichiers temporaires**

Une fois le traitement terminé, les fichiers temporaires sont supprimés.