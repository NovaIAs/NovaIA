```tcl
namespace eval algèbre {
    proc num {arg} {
        if {[expr {$arg =~ "^[-+]?[0-9]*\\.?[0-9]*$"]]} {
            return $arg
        } else {
            error "Argument [$arg] n'est pas un nombre"
        }
    }

    proc op {symbole} {
        if {[llength [lrange $symbole 0 1]] == 1} {
            return $symbole
        } else {
            error "Symbole d'opération [$symbole] invalide"
        }
    }

    proc paren {arg} {
        expr {$arg == "(" || $arg == ")"}
    }

    proc préfixe {arg} {
        expr {$arg == "-" || $arg == "+"}
    }

    proc postfixe {arg} {
        expr {$arg == "."}
    }

    proc séparateur {arg} {
        expr {$arg == " " || $arg == "\t" || $arg == "\n"}
    }

    proc préparer {expr} {
        set t [lreplace $expr [lsearch -inline -regexp " " $expr] end "" -replace]

        set st ""
        foreach c $t {
            if {[préfixe $c]} {
                if {[llength $st] > 0 && [préfixe [lindex $st end]]} {
                    append st [paren " "]
                }
                append st "($c"
            } elseif {[postfixe $c]} {
                append st ".)"
            } elseif {[separateur $c]} {
                continue
            } elseif {[paren $c]} {
                append st "$c"
            } else {
                append st [num $c]
            }
        }

        if {[llength $st] > 0 && [paren [lindex $st end]]} {
            append st ")"
        }

        return $st
    }

    proc parse {expr} {
        expr {[info exists :loop]}
        global :loop

        foreach c $expr {
            set :loop 0
            if {[op $c] || [préfixe $c] || [paren $c] || [postfixe $c]} {
                append $::stack $c
            } elseif {[num $c]} {
                set c [substr $c 0 -2]
                if {[llength $c] > 0} {
                    append $::pile $c
                }
            } elseif {[separateur $c]} {
                continue
            } else {
                error "Caractère [$c] invalide"
            }
        }

        if {[llength $::pile] > 0} {
            append $::stack [info script]
            set ::pile ""
        }
    }

    proc shunting {expr} {
        # Préparation de l'expression
        set expr [préparer $expr]

        # Analyse de l'expression
        parse $expr

        # Détermination de l'ordre des opérations
        set signe ""
        foreach c $::stack {
            if {[op $c]} {
                if {[llength $::file] == 0 || [op [lindex $::file end]] || [paren [lindex $::file end]]} {
                    append $::file $c
                } else {
                    append $::pile [lindex $::file end]
                    set ::file [lreplace $::file -1 end ""]
                    append $::file $c
                }
            } elseif {[paren $c]} {
                if {$c == ")"} {
                    while {[llength $::file] > 0 && [lindex $::file end] != "("} {
                        append $::pile [lindex $::file end]
                        set ::file [lreplace $::file -1 end ""]
                    }
                    if {[llength $::file] == 0} {
                        error "Parenthèse manquante"
                    }
                    set ::file [lreplace $::file -1 end ""]
                } else {
                    append $::file $c
                }
            } else {
                append $::pile $c
            }
        }

        while {[llength $::file] > 0} {
            append $::pile [lindex $::file end]
            set ::file [lreplace $::file -1 end ""]
        }

        # Retour de l'expression postfixée
        return $::pile
    }

    proc évaluer {expr} {
        set ::pile ""
        foreach op [shunting $expr] {
            if {[op $op] || [préfixe $op]} {
                set x [lindex $::pile end]
                set ::pile [lreplace $::pile -1 end ""]
                switch $op {
                    "+*" {
                        set y [lindex $::pile end]
                        set ::pile [lreplace $::pile -1 end ""]
                        switch $op {
                            "+" {
                                append $::pile [expr {$x + $y}]
                            }
                            "*" {
                                append $::pile [expr {$x * $y}]
                            }
                        }
                    }
                    "-" {
                        if {[préfixe $op]} {
                            append $::pile [expr {-1 * $x}]
                        } else {
                            set y [lindex $::pile end]
                            set ::pile [lreplace $::pile -1 end ""]
                            append $::pile [expr {$x - $y}]
                        }
                    }
                }
            } else {
                append $::pile $op
            }
        }

        return [lindex $::pile 0]
    }

    proc RacineCarrée {x} {
        return [expr {pow([expr {$x > 0}], 0.5)}]
    }

    proc Puissance {x y} {
        return [expr {pow($x, $y)}]
    }

    proc Factorielle {n} {
        if {[expr {$n < 0}]} {
            error "Factorielle non définie pour une valeur négative"
        }
        set f 1
        for {set i 1} {[expr {$i <= $n}]} {incr i} {
            set f [expr {$f * $i}]
        }
        return $f
    }

    proc Logarithme {x base} {
        return [expr {log($x) / log($base)}]
    }
}

algebra::évaluer "2 + 3 * (4 - 5)"
algebra::évaluer "sqrt(9)"
algebra::évaluer "pow(2, 3)"
algebra::évaluer "fact(5)"
algebra::évaluer "log(10, 100)"
```