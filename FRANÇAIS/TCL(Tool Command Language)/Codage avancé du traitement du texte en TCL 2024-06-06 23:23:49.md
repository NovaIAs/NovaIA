Voici un code Tcl complexe et étendu qui effectue un traitement avancé de texte :

```tcl
# Importation des bibliothèques nécessaires
package require Tcl 8.5
package require tclIO

# Initialisation des variables
texte = [read $stdin] # Récupération du texte depuis l'entrée standard
mots = {} # Dictionnaire pour stocker les mots et leurs occurrences
stop_words = {"est", "le", "la", "les", "de", "du", "à"} # Liste de mots vides

# Prétraitement du texte
texte = string tolower $texte # Conversion en minuscules
texte = [regexp -inline {[[:punct:]]} $texte " "] # Suppression des signes de ponctuation
texte = [regexp -inline {[[:digit:]]} $texte " "] # Suppression des chiffres

# Extraction et comptage des mots
foreach mot [split $texte " "] {
    if {![info exists mots($mot)]} {
        mots($mot) = 1
    } else {
        incr mots($mot)
    }
}

# Suppression des mots vides
foreach mot [array names mots] {
    if {$mot in $stop_words} {
        unset mots($mot)
    }
}

# Tri du dictionnaire par occurrences décroissantes
set m_trie [lsort -decreasing [lrange $mots 1 end 2]]

# Affichage des 10 mots les plus fréquents
puts "Les 10 mots les plus fréquents sont :"
for {set i 0} {$i < 10} {incr i} {
    set mot [lindex $m_trie $i]
    set occ [lindex $m_trie $i +1]
    puts "[format "%-15s %d" $mot $occ]"
}
```

### Explication du code :

- **Importation des bibliothèques** : Les bibliothèques `Tcl` et `tclIO` sont importées pour fournir des fonctionnalités étendues.

- **Initialisation des variables** : Le texte d'entrée est stocké dans la variable `texte`, un dictionnaire `mots` est initialisé pour compter les occurrences des mots, et une liste de `stop_words` (mots vides) est définie.

- **Prétraitement du texte** : Le texte est converti en minuscules, les signes de ponctuation et les chiffres sont supprimés pour normaliser le texte.

- **Extraction et comptage des mots** : La fonction `split` divise le texte en mots, qui sont ensuite comptés dans le dictionnaire `mots`.

- **Suppression des mots vides** : Les mots vides, qui n'apportent pas d'informations significatives, sont supprimés du dictionnaire.

- **Tri du dictionnaire** : Le dictionnaire `mots` est trié par occurrences décroissantes en utilisant la commande `lsort`.

- **Affichage des 10 mots les plus fréquents** : Les 10 mots les plus fréquents sont affichés avec leurs occurrences respectives. La commande `format` est utilisée pour formater la sortie.