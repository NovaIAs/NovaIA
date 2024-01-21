**TCL (Tool Command Language): Código Complejo en Español**

```tcl
# Crear una lista de nombres
nombres = {"Juan", "María", "Pedro", "Ana", "Luis"}

# Usar un bucle for para iterar sobre la lista
foreach nombre $nombres {
    # Imprimir un mensaje para cada nombre en la lista
    puts "Hola, $nombre!"
}

# Crear un diccionario para almacenar información sobre cada persona
personas = {}

# Usar un bucle for para iterar sobre la lista de nombres y crear una entrada en el diccionario para cada persona
foreach nombre $nombres {
    # Crear una nueva entrada en el diccionario para la persona
    personas($nombre) = {}

    # Añadir un campo "edad" a la entrada de la persona
    personas($nombre)("edad") = rand(18, 65)

    # Añadir un campo "dirección" a la entrada de la persona
    personas($nombre)("dirección") = "Calle ${nombre} 123"
}

# Imprimir el contenido del diccionario
foreach persona $personas {
    # Imprimir el nombre de la persona
    puts "Nombre: $persona"

    # Imprimir la edad de la persona
    puts "Edad: ${personas($persona)("edad")}"

    # Imprimir la dirección de la persona
    puts "Dirección: ${personas($persona)("dirección")}"

    # Imprimir una línea en blanco
    puts ""
}
```

**Explicación:**

1. **Lista de Nombres:** Se crea una lista de nombres utilizando la sintaxis `nombres = {"Juan", "María", "Pedro", "Ana", "Luis"}`.


2. **Bucle `for` para Iterar:** Se utiliza un bucle `for` para iterar sobre la lista de nombres y realizar una acción para cada nombre.


3. **Crear un Diccionario:** Se crea un diccionario utilizando la sintaxis `personas = {}`. Un diccionario es una estructura de datos que almacena pares clave-valor, y en este caso se utiliza para almacenar información sobre cada persona.


4. **Bucle `for` para Iterar sobre la Lista de Nombres:** Se utiliza un segundo bucle `for` para iterar sobre la lista de nombres y crear una entrada en el diccionario para cada persona.


5. **Crear una Nueva Entrada en el Diccionario:** Para crear una nueva entrada en el diccionario, se utiliza la sintaxis `personas($nombre) = {}`.


6. **Añadir Campos a la Entrada de la Persona:** Se añaden campos a la entrada de la persona utilizando la sintaxis `personas($nombre)("campo") = valor`. En este caso, se añaden dos campos: "edad" y "dirección".


7. **Imprimir el Contenido del Diccionario:** Se utiliza un bucle `foreach` para iterar sobre el diccionario y mostrar el nombre, la edad y la dirección de cada persona.


8. **Función `rand()` para Generar Edades Aleatorias:** La función `rand()` se utiliza para generar edades aleatorias entre 18 y 65 años.


9. **Referencias a Campos del Diccionario:** Para acceder a los campos de una entrada del diccionario, se utiliza la sintaxis `personas($nombre)("campo")`.