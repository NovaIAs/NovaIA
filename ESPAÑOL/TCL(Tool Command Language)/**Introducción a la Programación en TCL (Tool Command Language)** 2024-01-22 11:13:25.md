```tcl
# Declaración de variables
set nombre "Juan"
set edad 25
set nacionalidad "Española"

# Definición de una función para saludar
proc saludar {nombre} {
    puts "Hola, $nombre!"
}

# Invocación de la función
saludar $nombre

# Definición de una función para calcular el área de un círculo
proc area_circulo {radio} {
    return [expr {pi * $radio * $radio}]
}

# Invocación de la función
set radio 5
set area [area_circulo $radio]
puts "El área del círculo es: $area"

# Creación de una lista
set lista {1 2 3 4 5}

# Iteración sobre la lista
foreach elemento $lista {
    puts "Elemento: $elemento"
}

# Creación de un diccionario
set diccionario {
    "nombre" "Juan"
    "edad" 25
    "nacionalidad" "Española"
}

# Acceso a los valores del diccionario
puts "El nombre es: $diccionario(nombre)"
puts "La edad es: $diccionario(edad)"
puts "La nacionalidad es: $diccionario(nacionalidad)"

# Creación de una expresión regular
set expresion_regular "Hola, (.*)!"

# Búsqueda de coincidencias en una cadena de texto
set texto "Hola, Juan!"
if {[regexp $expresion_regular $texto -> nombre]} {
    puts "Nombre encontrado: $nombre"
}

# Generación de números aleatorios
set numero_aleatorio [rand 100]
puts "Número aleatorio: $numero_aleatorio"

# Ejecución de un comando del sistema
exec ls -l

# Creación de un timer
after 1000 {
    puts "Han pasado 1000 milisegundos"
}
```

Explicación del código:

* **Declaración de variables:** Se declaran varias variables, incluyendo el nombre de una persona, su edad y su nacionalidad.
* **Definición de una función:** Se define una función llamada `saludar` que recibe un nombre como argumento y muestra un saludo.
* **Invocación de la función:** Se invoca la función `saludar` pasándole el nombre de una persona como argumento.
* **Definición de otra función:** Se define una función llamada `area_circulo` que recibe el radio de un círculo como argumento y devuelve su área.
* **Invocación de la función:** Se invoca la función `area_circulo` pasándole el radio de un círculo como argumento y se almacena el resultado en una variable.
* **Creación de una lista:** Se crea una lista con varios elementos.
* **Iteración sobre la lista:** Se itera sobre la lista y se muestra cada elemento.
* **Creación de un diccionario:** Se crea un diccionario con varios pares clave-valor.
* **Acceso a los valores del diccionario:** Se accede a los valores del diccionario utilizando la sintaxis `$diccionario(clave)`.
* **Creación de una expresión regular:** Se crea una expresión regular que busca una cadena de texto que comienza con "Hola, " y termina con un signo de exclamación.
* **Búsqueda de coincidencias en una cadena de texto:** Se busca una coincidencia de la expresión regular en una cadena de texto y se almacena el resultado en una variable.
* **Generación de números aleatorios:** Se genera un número aleatorio entre 0 y 99.
* **Ejecución de un comando del sistema:** Se ejecuta el comando `ls -l` en el sistema operativo.
* **Creación de un timer:** Se crea un timer que se ejecutará después de 1000 milisegundos y mostrará un mensaje.