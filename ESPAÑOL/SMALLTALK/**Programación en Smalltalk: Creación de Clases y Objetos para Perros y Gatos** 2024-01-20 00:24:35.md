```smalltalk
"Definición de una clase Animal"

Clase Animal
    variablesInstancia
        nombre
        edad

    inicializarConNombreYEdad: unNombre unaEdad
        super inicializar.
        nombre := unNombre.
        edad := unaEdad.

    nombre
        ^nombre

    edad
        ^edad

    presentarse
        Transcript show: 'Soy ', nombre, ', tengo ', edad, ' años'.
```

```smalltalk
"Definición de una clase Perro, heredada de Animal"

Clase Perro
    hereda Animal

    variablesInstancia
        raza

    inicializarConNombreYEdadYRaza: unNombre unaEdad unaRaza
        super inicializarConNombreYEdad: unNombre unaEdad.
        raza := unaRaza.

    raza
        ^raza

    ladrar
        Transcript show: 'Guau!'
```

```smalltalk
"Definición de una clase Gato, heredada de Animal"

Clase Gato
    hereda Animal

    variablesInstancia
        color

    inicializarConNombreYEdadYColor: unNombre unaEdad unColor
        super inicializarConNombreYEdad: unNombre unaEdad.
        color := unColor.

    color
        ^color

    maullar
        Transcript show: 'Miau!'
```

```smalltalk
"Creación de un objeto Perro"

miPerro := Perro nuevo inicializarConNombreYEdadYRaza: 'Firulais' 5 'Golden Retriever'.

"Creación de un objeto Gato"

miGato := Gato nuevo inicializarConNombreYEdadYColor: 'Michi' 3 'Blanco'.
```

```smalltalk
"Invocación de métodos para los objetos Perro y Gato"

miPerro presentarse.
miGato presentarse.

miPerro ladrar.
miGato maullar.
```