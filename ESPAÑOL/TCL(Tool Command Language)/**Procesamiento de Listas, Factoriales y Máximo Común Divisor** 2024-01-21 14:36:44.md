```tcl
# Crear una lista de números
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Definir una función para calcular el factorial de un número
proc factorial {n} {
  if {$n <= 1} {
    return 1
  }
  return [expr {$n * [factorial [expr {$n - 1}]]}]
}

# Crear una lista de factoriales
lista_factoriales = map factorial $lista_numeros

# Imprimir la lista de factoriales
puts "Lista de números:"
foreach numero $lista_numeros {
  puts "$numero"
}
puts "Lista de factoriales:"
foreach factorial $lista_factoriales {
  puts "$factorial"
}

# Definir una función para calcular la suma de una lista de números
proc suma {lista} {
  if {[llength $lista] == 0} {
    return 0
  }
  return [expr {[lindex $lista 0] + [suma [lrange $lista 1 end]]}]
}

# Calcular la suma de la lista de factoriales
suma_factoriales = suma $lista_factoriales

# Imprimir la suma de los factoriales
puts "Suma de los factoriales: $suma_factoriales"

# Definir una función para calcular el máximo común divisor de dos números
proc mcd {a b} {
  if {$b == 0} {
    return $a
  }
  return [mcd $b [expr {$a % $b}]]
}

# Calcular el máximo común divisor de dos números grandes
numero1 = 123456789012345678901234567890
numero2 = 987654321098765432109876543210

mcd_numeros = mcd $numero1 $numero2

# Imprimir el máximo común divisor
puts "Máximo común divisor de $numero1 y $numero2: $mcd_numeros"

```

Explicación del código:

1. Creamos una lista de números del 1 al 10.
2. Definimos una función llamada `factorial` que calcula el factorial de un número dado.
3. Creamos una lista de factoriales aplicando la función `factorial` a cada número de la lista de números.
4. Imprimimos la lista de números y la lista de factoriales.
5. Definimos una función llamada `suma` que calcula la suma de una lista de números.
6. Calculamos la suma de la lista de factoriales usando la función `suma`.
7. Imprimimos la suma de los factoriales.
8. Definimos una función llamada `mcd` que calcula el máximo común divisor de dos números dados.
9. Calculamos el máximo común divisor de dos números grandes usando la función `mcd`.
10. Imprimimos el máximo común divisor de los dos números grandes.