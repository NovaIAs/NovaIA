**Code TCL complexe en français**

Ce code TCL complexe implémente un algorithme de recherche de chemin optimal utilisant l'heuristique A* dans un graphe pondéré. Il est conçu pour trouver le chemin le plus court entre deux nœuds dans un graphe, en tenant compte à la fois de la distance et des coûts associés aux arêtes.

```tcl
# Créer une liste de listes pour représenter le graphe
set graphe [list [list "A" "B" 1] [list "A" "C" 2] [list "A" "D" 4]
                [list "B" "C" 3] [list "B" "D" 2] [list "C" "D" 1]]

# Créer une fonction de calcul de coût d'arête
proc coût {arête} {
    return [lindex $arête 2]
}

# Créer une fonction de calcul de distance heuristique
proc distance {nœud1 nœud2} {
    # Calculer la distance à vol d'oiseau entre les nœuds
    set dx [expr {int([expr {$nœud1(x) - $nœud2(x)}])}]
    set dy [expr {int([expr {$nœud1(y) - $nœud2(y)}])}]
    set distance [expr {$dx * $dx + $dy * $dy}]
    return $distance
}

# Créer une fonction de recherche de chemin A*
proc astar {nœud_départ nœud_destination} {
    # Initialiser la file d'attente prioritaire et l'ensemble des nœuds visités
    set file [new PriorityQueue {key cost}]
    set visités {}

    # Enfiler le nœud de départ dans la file
    $file enqueue $nœud_départ 0

    # Boucle jusqu'à ce que la file soit vide ou que la destination soit atteinte
    while {$file size > 0} {
        # Défiler le nœud avec le coût le plus faible
        set nœud_courant [$file dequeue]

        # Marquer le nœud comme visité
        set visités($nœud_courant) 1

        # Vérifier si la destination est atteinte
        if {$nœud_courant == $nœud_destination} {
            return $nœud_courant
        }

        # Explorer les nœuds voisins
        foreach {voisin coût} [lsort -index 1 [lsearch $graphe $nœud_courant*]] {
            # Calculer le coût total et la distance heuristique
            set coût_total [expr {$coût + [coût $voisin]}]
            set distance_heuristique [distance $voisin $nœud_destination]

            # Enfiler le voisin dans la file si nécessaire
            if {! [exists visités($voisin)]} {
                $file enqueue $voisin $coût_total $distance_heuristique
            }
        }
    }

    # Aucun chemin trouvé
    return -1
}

# Testez l'algorithme en trouvant le chemin le plus court de A à D
set chemin [astar "A" "D"]

# Affichez le chemin trouvé
puts "Chemin le plus court de A à D : $chemin"
```

**Explication du code**

* La liste `graphe` représente le graphe sous forme de listes de triplets : `[nœud_source nœud_destination coût]`.
* La fonction `coût` renvoie le coût d'une arête donnée.
* La fonction `distance` calcule la distance heuristique (distance à vol d'oiseau) entre deux nœuds.
* La fonction `astar` implémente l'algorithme A* pour trouver le chemin optimal entre deux nœuds. Elle utilise une file d'attente prioritaire pour trier les nœuds à explorer en fonction de leur coût estimé.
* Le programme principal teste l'algorithme en trouvant le chemin le plus court de A à D dans le graphe et l'affiche.