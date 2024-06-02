**Code SHELL complexe et étendu**

```shell
#!/bin/bash

# Déclaration des variables globales
FICHIER_SOURCE="fichier_source.txt"
FICHIER_DESTINATION="fichier_destination.txt"
REGEX_MOT="mot_à_rechercher"

# Fonction pour rechercher un mot dans un fichier
recherche_mot() {
    grep "$REGEX_MOT" "$1"
}

# Fonction pour copier le contenu d'un fichier vers un autre
copier_fichier() {
    cat "$1" > "$2"
}

# Fonction pour afficher un message à l'écran
afficher_message() {
    echo "$1"
}

# Traitement principal
if [ -f "$FICHIER_SOURCE" ]; then
    # Si le fichier source existe, on recherche le mot
    MOT_TROUVE=$(recherche_mot "$FICHIER_SOURCE")
    if [ -n "$MOT_TROUVE" ]; then
        # Si le mot est trouvé, on copie le fichier vers la destination
        copier_fichier "$FICHIER_SOURCE" "$FICHIER_DESTINATION"
        afficher_message "Le fichier '$FICHIER_SOURCE' a été copié vers '$FICHIER_DESTINATION' car le mot '$REGEX_MOT' a été trouvé."
    else
        # Sinon, on affiche un message d'erreur
        afficher_message "Le mot '$REGEX_MOT' n'a pas été trouvé dans le fichier '$FICHIER_SOURCE'."
    fi
else
    # Si le fichier source n'existe pas, on affiche un message d'erreur
    afficher_message "Le fichier '$FICHIER_SOURCE' n'existe pas."
fi
```

**Explication du code**

Ce code SHELL complexe effectue les opérations suivantes :

* Il déclare des variables globales pour le fichier source, le fichier destination et le mot à rechercher.
* Il définit trois fonctions :
    * `recherche_mot()`: recherche un mot dans un fichier en utilisant `grep`.
    * `copier_fichier()`: copie le contenu d'un fichier vers un autre en utilisant `cat`.
    * `afficher_message()`: affiche un message à l'écran en utilisant `echo`.
* Dans le traitement principal :
    * Il vérifie si le fichier source existe.
    * S'il existe, il recherche le mot à l'aide de la fonction `recherche_mot()`.
    * Si le mot est trouvé, il copie le fichier vers la destination en utilisant la fonction `copier_fichier()`.
    * Sinon, il affiche un message d'erreur.
    * S'il n'existe pas, il affiche un message d'erreur.