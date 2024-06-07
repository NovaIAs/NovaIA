```tcl
# Déclarer une variable pour stocker les résultats
set résultats {}

# Créer une fonction pour calculer la factorielle d'un nombre
proc factorielle {n} {
    if {$n == 0} {
        return 1
    } else {
        return [expr {$n * [factorielle [expr {$n - 1}]]}]
    }
}

# Créer une fonction pour générer les nombres de Fibonacci
proc fibonacci {n} {
    if {$n <= 1} {
        return $n
    } else {
        return [expr {[fibonacci [expr {$n - 1}]] + [fibonacci [expr {$n - 2}]]}]
    }
}

# Créer une fonction pour vérifier si un nombre est premier
proc estPremier {n} {
    if {$n <= 1} {
        return 0
    } else {
        for {set i 2} {$i <= [expr {int(sqrt($n))}]} {incr i} {
            if {[expr {$n % $i == 0}]} {
                return 0
            }
        }
        return 1
    }
}

# Créer une fonction pour trouver les diviseurs d'un nombre
proc trouverDiviseurs {n} {
    for {set i 1} {$i <= $n} {incr i} {
        if {[expr {$n % $i == 0}]} {
            lappend résultats $i
        }
    }
    return $résultats
}

# Créer une fonction pour trouver le plus grand diviseur commun de deux nombres
proc trouverPGDC {a b} {
    while {$b != 0} {
        set r [expr {$a % $b}]
        set a $b
        set b $r
    }
    return $a
}

# Créer une fonction pour trouver le plus petit multiple commun de deux nombres
proc trouverPPMC {a b} {
    set pgdc [trouverPGDC $a $b]
    return [expr {$a * $b / $pgdc}]
}

# Exemples d'utilisation des fonctions

set factorielle10 [factorielle 10]
puts "Factorielle de 10 : $factorielle10"

set fibonacci10 [fibonacci 10]
puts "Nombre de Fibonacci n°10 : $fibonacci10"

set premier5 [estPremier 5]
set premier6 [estPremier 6]
puts "5 est premier : $premier5"
puts "6 est premier : $premier6"

set diviseurs12 [trouverDiviseurs 12]
puts "Diviseurs de 12 : $diviseurs12"

set pgdc12et18 [trouverPGDC 12 18]
puts "Plus grand diviseur commun de 12 et 18 : $pgdc12et18"

set ppmc12et18 [trouverPPMC 12 18]
puts "Plus petit multiple commun de 12 et 18 : $ppmc12et18"
```

**Explication du code:**

Ce code TCL implémente plusieurs fonctions mathématiques courantes :

* `factorielle` calcule la factorielle d'un nombre.
* `fibonacci` génère les nombres de Fibonacci.
* `estPremier` vérifie si un nombre est premier.
* `trouverDiviseurs` trouve les diviseurs d'un nombre.
* `trouverPGDC` trouve le plus grand diviseur commun de deux nombres.
* `trouverPPMC` trouve le plus petit multiple commun de deux nombres.

Le code utilise la variable `résultats` pour stocker les résultats des opérations.

Les exemples d'utilisation des fonctions montrent comment les appeler et afficher les résultats.