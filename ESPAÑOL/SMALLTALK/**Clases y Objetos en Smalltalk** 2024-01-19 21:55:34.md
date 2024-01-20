```smalltalk
"Clase Persona"

Clase Persona define:
    nombre      "nombre de la persona"
    edad         "edad de la persona"
    peso         "peso de la persona"

    constructor ["nombre", "edad", "peso"]
        nombre := nombre.
        edad := edad.
        peso := peso.

    toString
        "Devuelve una cadena de texto con la información de la persona"
        ^ 'Nombre: ' , nombre, '. Edad: ', edad, '. Peso: ', peso.

```

"Clase Estudiante"

Clase Estudiante define:
    escuela      "escuela a la que asiste el estudiante"
    curso         "curso que está cursando el estudiante"

    constructor ["nombre", "edad", "peso", "escuela", "curso"]
        super constructor ["nombre", "edad", "peso"].
        escuela := escuela.
        curso := curso.

    toString
        "Devuelve una cadena de texto con la información del estudiante"
        ^ super toString, '. Escuela: ', escuela, '. Curso: ', curso.

```

"Clase Profesor"

Clase Profesor define:
    asignatura      "asignatura que imparte el profesor"
    departamento     "departamento al que pertenece el profesor"

    constructor ["nombre", "edad", "peso", "asignatura", "departamento"]
        super constructor ["nombre", "edad", "peso"].
        asignatura := asignatura.
        departamento := departamento.

    toString
        "Devuelve una cadena de texto con la información del profesor"
        ^ super toString, '. Asignatura: ', asignatura, '. Departamento: ', departamento.

```

"Clase Director"

Clase Director define:
    salario         "salario del director"

    constructor ["nombre", "edad", "peso", "salario"]
        super constructor ["nombre", "edad", "peso"].
        salario := salario.

    toString
        "Devuelve una cadena de texto con la información del director"
        ^ super toString, '. Salario: ', salario.

```

"Creación de objetos"

persona1 := Persona constructor ["Juan", 20, 70].
estudiante1 := Estudiante constructor ["María", 18, 60, "Colegio San Juan", "1º de Bachillerato"].
profesor1 := Profesor constructor ["Pedro", 40, 80, "Matemáticas", "Departamento de Ciencias"].
director1 := Director constructor ["Antonio", 50, 90, 1000].

"Mostrar información de los objetos"

persona1 printString.
estudiante1 printString.
profesor1 printString.
director1 printString.

```

Explicación del código:

* Se definen cuatro clases: Persona, Estudiante, Profesor y Director.
* Cada clase tiene sus propios atributos y métodos.
* Los atributos se utilizan para almacenar información sobre los objetos.
* Los métodos se utilizan para realizar operaciones sobre los objetos.
* Se crean objetos de cada una de las clases.
* Se muestra información sobre los objetos.

El código es modular y reutilizable. Se pueden añadir nuevas clases y objetos fácilmente.