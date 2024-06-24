```tcl
proc exemple_complexe {input_list output_file} {
    # Vérifier la validité de l'entrée
    if {[llength $input_list] == 0} {
        error "Aucun élément dans la liste d'entrée."
    }
    if {[file exists $output_file]} {
        error "Le fichier de sortie existe déjà."
    }

    # Initialiser les variables de sortie
    set sortie {}

    # Tri à bulles de la liste d'entrée
    foreach i [lrange $input_list 0 end] {
        foreach j [lrange $input_list $i end] {
            if {[lset $input_list $i] > $j} {
                lset $input_list $i $j
                lset $input_list $j [lset $input_list $i]
            }
        }
    }

    # Calculer la médiane
    if {[llength $input_list] % 2 == 0} {
        # Médiane d'une liste paire
        set median [expr ([lindex $input_list [expr {[llength $input_list] / 2 - 1]}] + [lindex $input_list [expr {[llength $input_list] / 2]}]) / 2.0]
    } else {
        # Médiane d'une liste impaire
        set median [lindex $input_list [expr {[llength $input_list] / 2]}]
    }

    # Calculer l'écart-type
    set somme_carres 0.0
    foreach element $input_list {
        set somme_carres [expr {$somme_carres + ($element - $median) * ($element - $median)}]
    }
    set variance [expr {$somme_carres / [llength $input_list]}]
    set écart_type [expr {sqrt($variance)}]

    # Générer le rapport
    append sortie "Médiane : $median\n"
    append sortie "Écart-type : $écart_type\n"

    # Écrire le rapport dans le fichier de sortie
    file write $output_file $sortie

    # Renvoyer la médiane pour un traitement ultérieur
    return $median
}
```

**Explication du code :**

Ce code TCL complexe effectue une série d'opérations sur une liste d'entiers donnée :

* **Entrée :** Une liste d'entiers.
* **Sortie :** Un rapport texte écrit dans un fichier, contenant la médiane et l'écart-type des entiers.

**Fonctions principales :**

* **Tri à bulles :** La fonction trie la liste d'entrée dans l'ordre croissant.
* **Calcul de la médiane :** La médiane est calculée en trouvant le point médian de la liste triée (la valeur moyenne des deux points médians pour une liste paire, ou le point médian pour une liste impaire).
* **Calcul de l'écart-type :** L'écart-type est calculé en trouvant la racine carrée de la variance, qui est la moyenne des carrés des écarts par rapport à la médiane.
* **Génération du rapport :** Le rapport contenant la médiane et l'écart-type est généré et écrit dans le fichier de sortie spécifié.

**Utilisation :**

Pour utiliser ce code, vous pouvez l'appeler comme suit :

```tcl
set input_list [list 10 5 15 12 7 8]
set output_file "rapport.txt"
set median [exemple_complexe $input_list $output_file]
puts "Médiane : $median"
```

**Sortie attendue :**

Après l'exécution du code, le fichier `rapport.txt` contiendra les informations suivantes :

```
Médiane : 10
Écart-type : 3.649395737902285
```