```tcl
# Crear una lista de números
lista = {1 2 3 4 5 6 7 8 9 10}

# Definir una función para calcular el factorial de un número
proc factorial {n} {
  if {$n == 0} {
    return 1
  } else {
    return [expr {$n * [factorial [expr {$n - 1}]]}]
  }
}

# Crear una nueva lista con los factoriales de los números de la lista original
lista_factoriales = {}
foreach num $lista {
  lappend lista_factoriales [factorial $num]
}

# Imprimir la lista original y la lista de factoriales
puts "Lista original: $lista"
puts "Lista de factoriales: $lista_factoriales"

# Ordenar la lista de factoriales en orden ascendente
lista_factoriales = [lsort -integer $lista_factoriales]

# Imprimir la lista de factoriales ordenada
puts "Lista de factoriales ordenada: $lista_factoriales"

# Obtener el primer y último elemento de la lista de factoriales
primer_factorial = [lindex $lista_factoriales 0]
ultimo_factorial = [lindex $lista_factoriales [llength $lista_factoriales] -1]

# Imprimir el primer y último factorial
puts "Primer factorial: $primer_factorial"
puts "Último factorial: $ultimo_factorial"

# Crear una nueva lista con los números de la lista original que son pares
lista_pares = {}
foreach num $lista {
  if {$num % 2 == 0} {
    lappend lista_pares $num
  }
}

# Imprimir la lista de números pares
puts "Lista de números pares: $lista_pares"

# Crear una nueva lista con los números de la lista original que son impares
lista_impares = {}
foreach num $lista {
  if {$num % 2 != 0} {
    lappend lista_impares $num
  }
}

# Imprimir la lista de números impares
puts "Lista de números impares: $lista_impares"

# Crear una nueva lista con los números de la lista original que son primos
lista_primos = {}
foreach num $lista {
  if {[isPrime $num]} {
    lappend lista_primos $num
  }
}

# Imprimir la lista de números primos
puts "Lista de números primos: $lista_primos"

# Definir una función para verificar si un número es primo
proc isPrime {n} {
  if {$n < 2} {
    return 0
  }
  for {set i 2} {$i * $i <= $n} {incr i} {
    if {$n % $i == 0} {
      return 0
    }
  }
  return 1
}

# Crear una nueva lista con los números de la lista original que son perfectos
lista_perfectos = {}
foreach num $lista {
  if {[isPerfect $num]} {
    lappend lista_perfectos $num
  }
}

# Imprimir la lista de números perfectos
puts "Lista de números perfectos: $lista_perfectos"

# Definir una función para verificar si un número es perfecto
proc isPerfect {n} {
  if {$n < 6} {
    return 0
  }
  set suma_divisores 0
  for {set i 1} {$i < $n} {incr i} {
    if {$n % $i == 0} {
      set suma_divisores [expr {$suma_divisores + $i}]
    }
  }
  if {$suma_divisores == $n} {
    return 1
  } else {
    return 0
  }
}
```

Explicación del código:

* Creamos una lista de números del 1 al 10 y la imprimimos.
* Definimos una función para calcular el factorial de un número.
* Creamos una nueva lista con los factoriales de los números de la lista original y la imprimimos.
* Ordenamos la lista de factoriales en orden ascendente y la imprimimos.
* Obtenemos el primer y último elemento de la lista de factoriales y los imprimimos.
* Creamos una nueva lista con los números de la lista original que son pares y la imprimimos.
* Creamos una nueva lista con los números de la lista original que son impares y la imprimimos.
* Creamos una nueva lista con los números de la lista original que son primos y la imprimimos.
* Definimos una función para verificar si un número es primo.
* Creamos una nueva lista con los números de la lista original que son perfectos y la imprimimos.
* Definimos una función para verificar si un número es perfecto.