```tcl
# Función para calcular el factorial de un número
proc factorial {num} {
  if {$num == 0} {
    return 1
  } else {
    return [expr {$num * [factorial [expr {$num - 1}]]}]
  }
}

# Función para imprimir una tabla de multiplicar
proc tablaMultiplicar {num} {
  puts "Tabla de multiplicar de $num:"
  for {set i 1} {$i <= 10} {incr i} {
    puts "[expr {$num * $i}]"
  }
}

# Función para obtener el mínimo común múltiplo de dos números
proc mcm {num1 num2} {
  set producto [expr {$num1 * $num2}]
  set mcd [mcd $num1 $num2]
  return [expr {$producto / $mcd}]
}

# Función para obtener el máximo común divisor de dos números
proc mcd {num1 num2} {
  if {$num1 == $num2} {
    return $num1
  } else {
    if {$num1 > $num2} {
      return [mcd [expr {$num1 - $num2}] $num2]
    } else {
      return [mcd $num1 [expr {$num2 - $num1}]]
    }
  }
}

# Función para resolver una ecuación cuadrática
proc resolverEcuacionCuadratica {a b c} {
  set discriminante [expr {$b * $b - 4 * $a * $c}]
  if {$discriminante < 0} {
    puts "No hay soluciones reales."
  } else {
    set raiz1 [expr {(-$b + [sqrt $discriminante]) / (2 * $a)}]
    set raiz2 [expr {(-$b - [sqrt $discriminante]) / (2 * $a)}]
    puts "Las soluciones son: $raiz1, $raiz2"
  }
}

# Función para imprimir un patrón de triángulo
proc imprimirTriangulo {numFilas} {
  for {set i 1} {$i <= $numFilas} {incr i} {
    for {set j 1} {$j <= $i} {incr j} {
      puts "*"
    }
    puts ""
  }
}

# Función para imprimir un patrón de rombo
proc imprimirRombo {numFilas} {
  # Imprimir la parte superior del rombo
  for {set i 1} {$i <= $numFilas} {incr i} {
    for {set j 1} {$j <= $i} {incr j} {
      puts " "
    }
    for {set j 1} {$j <= [expr {2 * ($numFilas - $i)}]} {incr j} {
      puts "*"
    }
    puts ""
  }

  # Imprimir la parte inferior del rombo
  for {set i 1} {$i < $numFilas} {incr i} {
    for {set j 1} {$j <= [expr {$numFilas - $i}]} {incr j} {
      puts " "
    }
    for {set j 1} {$j <= [expr {2 * $i}]} {incr j} {
      puts "*"
    }
    puts ""
  }
}

# Función para imprimir un patrón de árbol de Navidad
proc imprimirArbolNavidad {numFilas} {
  # Imprimir la parte superior del árbol
  for {set i 1} {$i <= $numFilas} {incr i} {
    for {set j 1} {$j <= [expr {$numFilas - $i}]} {incr j} {
      puts " "
    }
    for {set j 1} {$j <= [expr {2 * $i - 1}]} {incr j} {
      puts "*"
    }
    puts ""
  }

  # Imprimir el tronco del árbol
  for {set i 1} {$i <= 2} {incr i} {
    for {set j 1} {$j <= [expr {$numFilas - 1}]} {incr j} {
      puts " "
    }
    puts "||"
  }
}

# Función para imprimir un patrón de rectángulo
proc imprimirRectangulo {numFilas numColumnas} {
  for {set i 1} {$i <= $numFilas} {incr i} {
    for {set j 1} {$j <= $numColumnas} {incr j} {
      puts "*"
    }
    puts ""
  }
}

# Función para imprimir un patrón de rombo hueco
proc imprimirRomboHueco {numFilas} {
  # Imprimir la parte superior del rombo
  for {set i 1} {$i <= $numFilas} {incr i} {
    for {set j 1} {$j <= [expr {$numFilas - $i}]} {incr j} {
      puts " "
    }
    puts "*"
    for {set j 1} {$j <= [expr {2 * ($numFilas - $i) - 2}]} {incr j} {
      puts " "
    }
    puts "*"
    puts ""
  }

  # Imprimir la parte inferior del rombo
  for {set i 1} {$i < $numFilas} {incr i} {
    for {set j 1} {$j <= [expr {$numFilas - $i}]} {incr j} {
      puts " "
    }
    puts "*"
    for {set j 1} {$j <= [expr {2 * $i - 2}]} {incr j} {
      puts