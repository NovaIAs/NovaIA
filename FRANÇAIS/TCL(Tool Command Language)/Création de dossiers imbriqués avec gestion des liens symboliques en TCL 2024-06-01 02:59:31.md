```tcl
proc creerDossier {dossier} {
  if {[file exists $dossier]} {
    puts stderr "Le dossier $dossier existe déjà."
    return
  }

  # Créer le dossier
  file mkdir $dossier

  # Obtenir la liste des fichiers et dossiers dans le dossier parent
  set fichiers [file ls [file dirname $dossier]]

  # Boucler sur les fichiers et dossiers
  foreach fichier $fichiers {
    # Vérifier si le fichier ou dossier est un lien symbolique
    set attributs [file attributes $fichier]
    if {[lindex $attributs 8] == "l"} {
      # Résoudre le lien symbolique
      set lien [file readlink $fichier]

      # S'il s'agit d'un lien vers un dossier, le créer dans le nouveau dossier
      if {[file isdirectory $lien]} {
        creerDossier [file join $dossier [file name $lien]]
      }
    }
  }
}

# Exemple d'utilisation : créer un dossier imbriqué avec des liens symboliques
creerDossier /monDossier/sousDossier1
file symlink /monDossier/sousDossier1/fichier1 /lienVersFichier1
```

**Explication du code :**

Ce code est divisé en deux parties :

1. La procédure `creerDossier` crée un nouveau dossier et copie récursivement tous les liens symboliques dans le dossier parent vers le nouveau dossier.

2. L'exemple d'utilisation crée un dossier imbriqué `/monDossier/sousDossier1` et un lien symbolique `lienVersFichier1` qui pointe vers `/monDossier/sousDossier1/fichier1`.

Voici une explication détaillée du code étape par étape :

* La procédure `creerDossier` prend un argument `dossier`, qui est le chemin d'accès au dossier à créer.

* Si le dossier existe déjà, la procédure affiche un message d'erreur et retourne.

* La procédure utilise la commande `file mkdir` pour créer le dossier.

* Elle obtient ensuite la liste des fichiers et dossiers dans le dossier parent à l'aide de la commande `file ls`.

* Elle boucle sur chaque fichier et dossier de la liste à l'aide de la commande `foreach`.

* Pour chaque fichier ou dossier, elle vérifie s'il s'agit d'un lien symbolique à l'aide de la commande `file attributes`.

* Si le fichier ou dossier est un lien symbolique, elle résout le lien à l'aide de la commande `file readlink`.

* Si le lien résolu pointe vers un dossier, elle crée le dossier dans le nouveau dossier à l'aide de la commande `creerDossier`.

* L'exemple d'utilisation crée un dossier imbriqué `/monDossier/sousDossier1` à l'aide de la procédure `creerDossier`.

* Il crée ensuite un lien symbolique `lienVersFichier1` qui pointe vers `/monDossier/sousDossier1/fichier1` à l'aide de la commande `file symlink`.