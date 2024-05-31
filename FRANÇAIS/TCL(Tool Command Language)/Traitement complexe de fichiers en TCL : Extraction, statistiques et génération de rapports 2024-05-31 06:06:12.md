**Script complexe de traitement de fichiers en TCL**

Ce script TCL fournit un exemple complexe de traitement de fichiers, de manipulation de données et de contrôle de flux. Il effectue les tâches suivantes :

* Lit plusieurs fichiers texte source.
* Extrait des données structurées de chaque fichier.
* Calcule des statistiques sur les données extraites.
* Génère un rapport de synthèse.

**Code TCL**

```tcl
#!/usr/bin/wish

# Définir les constantes
set DIR_INPUT  "/chemin/vers/fichiers/source"
set DIR_OUTPUT "/chemin/vers/rapport"
set PATTERN    "*.txt"

# Obtenir la liste des fichiers source
# "glob" renvoie une liste de fichiers correspondant au modèle
set fichiers [glob -directory $DIR_INPUT -pattern $PATTERN]

# Initialiser les structures de données pour les statistiques
set stats {compteur 0 total 0 moyenne 0}

# Boucler sur chaque fichier source
foreach fichier $fichiers {
    # Lire et analyser le contenu du fichier
    # "open" ouvre le fichier pour la lecture
    # "gets" lit une ligne du fichier
    # "regexp" analyse la ligne pour extraire les données
    set fd [open $fichier r]
    while {[gets $fd ligne] != -1} {
        if {regexp {([0-9]+)} $ligne match} {
            # Stocker la valeur extraite dans la structure "stats"
            incr stats(compteur)
            set stats(total) [expr {$stats(total) + $match(1)}]
        }
    }
    close $fd
}

# Calculer la moyenne des données
set stats(moyenne) [expr {$stats(total) / $stats(compteur)}]

# Générer le rapport de synthèse
set rapport [open [file join $DIR_OUTPUT rapport.txt] w]
puts $rapport "Rapport de synthèse"
puts $rapport "-----------------"
puts $rapport "Compteur total : $stats(compteur)"
puts $rapport "Total des valeurs : $stats(total)"
puts $rapport "Moyenne : $stats(moyenne)"
close $rapport
```

**Explication du code**

* Le script commence par importer la bibliothèque TCL standard.
* Il définit des constantes pour les répertoires d'entrée et de sortie, ainsi que le modèle de fichier.
* Il obtient la liste des fichiers source à l'aide de la commande "glob".
* Il boucle sur chaque fichier source.
* Pour chaque fichier, il l'ouvre pour la lecture et lit chaque ligne.
* Il analyse chaque ligne à l'aide de la commande "regexp" pour extraire les données structurées.
* Il stocke les données extraites dans la structure de données "stats".
* Après avoir traité tous les fichiers, il calcule la moyenne des données.
* Enfin, il génère un rapport de synthèse dans un fichier texte, résumant les statistiques collectées.