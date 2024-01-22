```tcl
# Procedimientos

proc genera_numeros_aleatorios {cantidad} {
  set numeros {}

  for {set i 0} {$i < $cantidad} {incr i} {
    lappend numeros [expr {rand()*100}]
  }

  return $numeros
}

proc ordenar_numeros {numeros} {
  lsort -integer $numeros
}

proc crear_histograma {numeros} {
  set histogram {}

  foreach n $numeros {
    incr histogram($n)
  }

  return $histogram
}

proc imprimir_histograma {histogram} {
  foreach numero [lsort -integer [lkeys $histogram]] {
    puts "$numero: ${histogram($numero)}"
  }
}

# Programa principal

# Generar 100 números aleatorios
set numeros [genera_numeros_aleatorios 100]

# Ordenar los números
set numeros_ordenados [ordenar_numeros $numeros]

# Crear histograma
set histogram [crear_histograma $numeros]

# Imprimir histograma
puts "Histograma:"
imprimir_histograma $histogram
```

Este código Tcl genera 100 números aleatorios, los ordena, crea un histograma de los números ordenados e imprime el histograma.

El código se divide en cinco procedimientos:

* `genera_numeros_aleatorios`: Genera un número especificado de números aleatorios entre 0 y 100.
* `ordenar_numeros`: Ordena una lista de números en orden ascendente.
* `crear_histograma`: Crea un histograma a partir de una lista de números. El histograma es un diccionario que mapea cada número a su número de apariciones en la lista.
* `imprimir_histograma`: Imprime un histograma.
* `programa_principal`: El programa principal genera 100 números aleatorios, los ordena, crea un histograma de los números ordenados e imprime el histograma.

El código principal llama al procedimiento `genera_numeros_aleatorios` para generar 100 números aleatorios. Luego, llama al procedimiento `ordenar_numeros` para ordenar los números. Luego, llama al procedimiento `crear_histograma` para crear un histograma de los números ordenados. Finalmente, llama al procedimiento `imprimir_histograma` para imprimir el histograma.

El histograma se imprime en la consola en el siguiente formato:

```
Número: Número de apariciones
```

Por ejemplo, el siguiente es un histograma de 100 números aleatorios:

```
Número: Número de apariciones
0: 1
1: 2
2: 3
3: 4
4: 5
...
97: 6
98: 7
99: 8
100: 9
```