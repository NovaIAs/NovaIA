**Code TCL complexe en français**

```tcl
# Créer une liste de chaînes de caractères
liste_chaines = {"Bonjour", "monde", "cruel", "!"}

# Parcourir la liste et afficher chaque chaîne sur une ligne séparée
foreach chaine $liste_chaines {
    puts $chaine
}

# Créer un dictionnaire avec des clés de type chaîne et des valeurs de type entier
dico_entier = [dict create {}]
dict set dico_entier "un" 1
dict set dico_entier "deux" 2
dict set dico_entier "trois" 3

# Parcourir le dictionnaire et afficher chaque paire clé-valeur
foreach clef [dict keys $dico_entier] {
    puts "$clef => [dict get $dico_entier $clef]"
}

# Créer une procédure qui prend un nombre comme argument et renvoie son carré
proc carre {nombre} {
    expr {$nombre * $nombre}
}

# Appeler la procédure avec un nombre et afficher le résultat
puts "[carre 5]"

# Créer une expression régulière pour rechercher des mots commençant par un "a"
expr_regex = regexp "a.*"

# Utiliser l'expression régulière pour rechercher des mots dans une chaîne
if {[regexp $expr_regex "arbre"] != {}] {
    puts "Le mot 'arbre' commence par la lettre 'a'"
}

# Créer un canal de fichier pour écrire des données dans un fichier
canal_fichier = open "exemple.txt" w
# Écrire des données dans le fichier
puts -nonewline $canal_fichier "Ceci est une ligne de texte"
# Fermer le canal de fichier
close $canal_fichier

# Créer un widget fenêtre
fenetre = [toplevel]
# Créer un widget bouton dans la fenêtre
bouton = [button $fenetre -text "OK" -command {puts "Bouton OK cliqué"}]
# Emballer le bouton dans la fenêtre
pack $bouton

# Lancer la boucle d'événements pour exécuter l'interface graphique
mainloop
```

**Explication du code**

Ce code TCL effectue diverses opérations complexes, notamment :

* Créer et parcourir une liste
* Créer et parcourir un dictionnaire
* Définir une procédure avec un argument et la renvoyer
* Utiliser une expression régulière pour rechercher dans une chaîne
* Écrire des données dans un fichier
* Créer une interface graphique simple avec un bouton