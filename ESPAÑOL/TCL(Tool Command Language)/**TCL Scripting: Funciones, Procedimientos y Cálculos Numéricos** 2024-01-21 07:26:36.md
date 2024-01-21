```tcl
# Definición de la función "saludo".
proc saludo {nombre} {
    # Se utiliza la función "puts" para mostrar un mensaje en la consola.
    puts "Hola, $nombre. ¿Cómo estás?"
}

# Se crea una lista de nombres.
lista_nombres = [Juan Pedro María Ana]

# Se itera sobre la lista de nombres y se llama a la función "saludo" para cada nombre.
foreach nombre $lista_nombres {
    saludo $nombre
}

# Se define un procedimiento para generar un número aleatorio.
proc generar_aleatorio {min max} {
    expr {int(rand() * ($max - $min + 1)) + $min}
}

# Se utiliza el procedimiento para generar un número aleatorio entre 1 y 100.
numero_aleatorio = generar_aleatorio 1 100

# Se muestra el número aleatorio en la consola.
puts "El número aleatorio generado es: $numero_aleatorio"

# Se define una función para calcular el factorial de un número.
proc factorial {n} {
    # Si n es igual a 0, se devuelve 1.
    if {$n == 0} {
        return 1
    }

    # En caso contrario, se calcula el factorial recursivamente.
    return expr {$n * factorial($n - 1)}
}

# Se calcula el factorial de 5.
factorial_5 = factorial 5

# Se muestra el factorial de 5 en la consola.
puts "El factorial de 5 es: $factorial_5"

# Se define una función para calcular la suma de los primeros n números naturales.
proc suma_naturales {n} {
    # Si n es igual a 0, se devuelve 0.
    if {$n == 0} {
        return 0
    }

    # En caso contrario, se calcula la suma recursivamente.
    return expr {$n + suma_naturales($n - 1)}
}

# Se calcula la suma de los primeros 10 números naturales.
suma_10_naturales = suma_naturales 10

# Se muestra la suma de los primeros 10 números naturales en la consola.
puts "La suma de los primeros 10 números naturales es: $suma_10_naturales"
```

Explicación del código:

* La primera parte del código define una función llamada "saludo" que toma un nombre como argumento y muestra un mensaje de saludo en la consola.
* La segunda parte del código crea una lista de nombres y llama a la función "saludo" para cada nombre de la lista.
* La tercera parte del código define un procedimiento llamado "generar_aleatorio" que toma un mínimo y un máximo como argumentos y genera un número aleatorio entre esos valores.
* La cuarta parte del código utiliza el procedimiento para generar un número aleatorio entre 1 y 100 y lo muestra en la consola.
* La quinta parte del código define una función llamada "factorial" que toma un número como argumento y calcula su factorial.
* La sexta parte del código calcula el factorial de 5 y lo muestra en la consola.
* La séptima parte del código define una función llamada "suma_naturales" que toma un número como argumento y calcula la suma de los primeros n números naturales.
* La octava parte del código calcula la suma de los primeros 10 números naturales y lo muestra en la consola.