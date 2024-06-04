```tcl
# Ce programme implémente un algorithme de tri par insertion en TCL.

# La procédure suivante trie un tableau en place à l'aide de l'algorithme de tri par insertion.
proc insertionSort {tableau} {
    set n [llength $tableau]

    for {set i 1} {$i < $n} {incr i} {
        set el [lindex $tableau $i]
        set j [expr {$i - 1}]

        while {$j >= 0 && [expr {$el < [lindex $tableau $j]}]} {
            lset tableau $j+1 [lindex $tableau $j]
            decr j
        }

        lset tableau $j+1 $el
    }

    return $tableau
}

# Test du programme
set tableau [list 5 1 4 2 8 3 7 6]
puts "Tableau original : $tableau"

set tableau [insertionSort $tableau]
puts "Tableau trié : $tableau"
```

**Explication du code :**

* La procédure `insertionSort` prend un tableau en entrée et le trie en place à l'aide de l'algorithme de tri par insertion.

* L'algorithme de tri par insertion fonctionne en insérant chaque élément du tableau dans la bonne position dans la partie triée du tableau.

* La boucle `for` itère sur chaque élément du tableau.

* Pour chaque élément, une boucle `while` est utilisée pour trouver la position correcte de l'élément dans la partie triée du tableau.

* L'élément est inséré à sa position correcte en déplaçant les éléments plus grands vers la droite.

* Le tableau trié est renvoyé par la procédure.

* Le code de test crée un tableau initial, l'affiche, trie le tableau à l'aide de la procédure `insertionSort` et affiche le tableau trié.