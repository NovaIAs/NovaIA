**Calcul des racines d'un polynôme complexe**

Voici un code TCL complexe qui calcule les racines d'un polynôme complexe :

```tcl
proc racines_complexes {poly} {
    # Extraire les coefficients du polynôme
    set coeffs [lrange $poly 0 end]

    # Initialiser la liste des racines
    set racines {}

    # Calculer les racines complexes
    for {set i 1} {$i <= len($coeffs)} {incr i} {
        set c0 [lindex $coeffs 0]
        set c1 [lindex $coeffs 1]
        set discriminant [expr {$c1*$c1 - 4*$c0*[lindex $coeffs 2]}]
        if {$discriminant < 0} {
            # Racines complexes
            set racines [lconcat $racines [list [expr {$c1/(2*$c0)}] [expr {$c1/(2*$c0)}]i]]
        } elseif {$discriminant == 0} {
            # Racine réelle double
            set racines [lconcat $racines [list [expr {$c1/(2*$c0)}]]]
        } else {
            # Racines réelles distinctes
            set racines [lconcat $racines [list [expr {$c1 - $discriminant**0.5/(2*$c0)}] [expr {$c1 + $discriminant**0.5/(2*$c0)}]]]
        }
    }

    # Renvoyer les racines
    return $racines
}
```

**Explication du code :**

* La procédure `racines_complexes` prend un polynôme complexe sous forme de liste de coefficients comme argument.
* La procédure extrait les coefficients du polynôme à l'aide de la commande `lrange`.
* Elle initialise une liste vide `racines` pour stocker les racines.
* La procédure parcourt les coefficients du polynôme et calcule les racines complexes pour chaque coefficient à l'aide des formules quadratiques.
* Elle utilise la commande `lindex` pour extraire les coefficients pertinents.
* Elle calcule le discriminant pour déterminer le type de racines (complexes, réelles doubles ou réelles distinctes).
* La procédure ajoute les racines à la liste `racines` en fonction du type de racines.
* Enfin, elle renvoie la liste des racines.