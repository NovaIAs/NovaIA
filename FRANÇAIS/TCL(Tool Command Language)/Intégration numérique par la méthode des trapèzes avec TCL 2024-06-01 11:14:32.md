```tcl
proc calculerIntegrale x0 x1 fcn
    variable etape
    variable somme

    set etape (abs($x1 - $x0) / 1000)  ; Nombre d'étapes pour l'intégration

    set i 0
    while {$i <= 1000} {
        incr i
        set xi [expr {$x0 + $i * $etape}]
        set somme [expr {$somme + $etape * $fcn($xi)}]
    }

    return $somme
end

proc fcn x
    return [expr {sin(x) - cos(x)}]
end

set x0 0
set x1 1
set resultat [calculerIntegrale $x0 $x1 fcn]

puts "L'intégrale de sin(x) - cos(x) entre 0 et 1 par la méthode des trapèzes est :\n$resultat"
```
**Explication du code :**

Cette procédure TCL calcule l'intégrale définie de `sin(x) - cos(x)` sur l'intervalle [0, 1] en utilisant la méthode des trapèzes. Voici une explication détaillée :

* La procédure `calculerIntegrale` prend trois arguments : `x0` et `x1` qui sont les limites inférieure et supérieure de l'intégrale, et `fcn` qui est la fonction à intégrer.
* Elle initialise trois variables : `etape` qui représente la taille des intervalles, `somme` qui accumule les aires des trapèzes et `i` qui est un compteur d'itérations.
* La boucle `while` calcule la somme des aires des trapèzes. Pour chaque incrément de `i`, elle calcule la valeur de `x` à ce point (notée `xi`) et ajoute l'aire du trapèze correspondant à `somme`.
* La fonction `fcn` est utilisée pour évaluer la valeur de la fonction `sin(x) - cos(x)` à chaque point `xi`.
* Enfin, la procédure renvoie la valeur de `somme` qui représente l'intégrale approchée de la fonction sur l'intervalle donné.

Dans le code principal, les limites `x0` et `x1` sont initialisées à 0 et 1, respectivement. La procédure `calculerIntegrale` est appelée avec ces limites et la fonction `fcn`, et le résultat est affecté à `resultat`.

La valeur de `resultat` est ensuite affichée à l'écran, fournissant l'intégrale approchée de la fonction `sin(x) - cos(x)` sur l'intervalle [0, 1].