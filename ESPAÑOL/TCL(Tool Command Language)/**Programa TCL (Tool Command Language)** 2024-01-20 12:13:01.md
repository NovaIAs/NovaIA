```tcl
# Definición de variables

set nombre "Juan Pérez"
set edad 25
set ciudad "Barcelona"

# Definición de una lista

set lista {1 2 3 4 5}

# Definición de un diccionario

set diccionario {
    nombre "Juan Pérez"
    edad 25
    ciudad "Barcelona"
}

# Imprimir en pantalla

puts "Nombre:", $nombre
puts "Edad:", $edad
puts "Ciudad:", $ciudad

# Recorrer una lista

foreach item $lista {
    puts $item
}

# Recorrer un diccionario

foreach key value $diccionario {
    puts "$key: $value"
}

# Condicionales

if {$edad >= 18} {
    puts "Eres mayor de edad."
} elseif {$edad >= 13} {
    puts "Eres adolescente."
} else {
    puts "Eres niño."
}

# Bucles

for {set i 0} {$i < 10} {incr i} {
    puts $i
}

while {$i < 10} {
    puts $i
    incr i
}

# Funciones

proc saludar {nombre} {
    puts "Hola, $nombre!"
}

saludar "Juan Pérez"

# Procedimientos

proc imprimirLista {lista} {
    foreach item $lista {
        puts $item
    }
}

imprimirLista $lista

# Tratamiento de errores

set dato "10Z"

try {
    set numero [expr $dato]
} on error {msg result code} {
    puts "Error al convertir a número: $msg"
}

# Captura de eventos

event generate .button-1
```

**Explicación del código:**

* El código comienza definiendo varias variables, incluyendo una cadena de caracteres, un número y una lista.
* A continuación, se define un diccionario, que es un tipo de variable que puede almacenar pares de clave-valor.
* Se utilizan las instrucciones `puts` para imprimir en pantalla el valor de las variables.
* El código también incluye un bucle `foreach` para recorrer una lista y un diccionario.
* Se utilizan instrucciones condicionales para tomar decisiones en función del valor de una variable.
* El código también incluye un bucle `for` y un bucle `while`.
* Se define una función llamada `saludar` que recibe un nombre como argumento y lo imprime en pantalla.
* Se define un procedimiento llamado `imprimirLista` que recibe una lista como argumento y la imprime en pantalla.
* El código también incluye un gestor de errores para capturar errores que puedan producirse al ejecutar el programa.
* Por último, el código también incluye una instrucción para capturar eventos del ratón.