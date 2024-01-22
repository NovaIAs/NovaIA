```smalltalk
"Definición de la clase"
Object subclass: [Persona]

"Creación de instancias de la clase"
Persona nueva crear: 'Juan' apellido: 'Pérez' edad: 25.
Persona nueva crear: 'María' apellido: 'Gómez' edad: 30.

"Definición de métodos de instancia"
Persona métodos añadir: [
    imprimirNombre ->
        nombre imprimir.
        apellido imprimir.
    incrementarEdad ->
        edad := edad + 1.
]

"Definición de métodos de clase"
Persona métodos de clase añadir: [
    listarPersonas ->
        personas do: [:persona | persona imprimirNombre].
]

"Envío de mensajes a instancias de la clase"
Juan imprimirNombre.
María imprimirNombre.

"Envío de mensaje a la clase"
Persona listarPersonas.
```

Explicación:

* La primera línea define una nueva clase llamada "Persona" que se basa en la clase "Object".
* Las siguientes dos líneas crean dos instancias de la clase "Persona" con los nombres "Juan" y "María", los apellidos "Pérez" y "Gómez" y las edades 25 y 30 respectivamente.
* La siguiente sección define dos métodos de instancia para la clase "Persona": "imprimirNombre" e "incrementarEdad".
* El método "imprimirNombre" simplemente imprime el nombre y el apellido de la persona.
* El método "incrementarEdad" incrementa la edad de la persona.
* La siguiente sección define un método de clase para la clase "Persona": "listarPersonas".
* El método "listarPersonas" recorre todas las personas creadas e imprime sus nombres.
* La última parte del código envía mensajes a instancias de la clase "Persona" y a la propia clase.