```smalltalk
"Definiendo la interfaz de la clase 'Persona'"
Persona >> [
    "Propiedades"
    nombre: ""
    edad: 0

    "Constructor"
    nuevo [nombre: unNombre edad: unaEdad]
        "Crea una nueva instancia de 'Persona' con los valores especificados."
        ^ self nuevo initialize: unNombre edad: unaEdad

    "Métodos"
    initialize [unNombre unaEdad]
        "Inicializa la instancia con los valores especificados."
        nombre := unNombre.
        edad := unaEdad.

    saludo []
        "Devuelve un saludo de la persona."
        ^ 'Hola, me llamo ' , self nombre , ' y tengo ' , self edad , ' años.'
]

"Definiendo la clase 'Estudiante'"
Estudiante >> Persona [
    "Propiedades"
    calificacion: 0

    "Constructor"
    nuevo [unNombre unaEdad unaCalificacion]
        "Crea una nueva instancia de 'Estudiante' con los valores especificados."
        ^ self nuevo initialize: unNombre edad: unaEdad calificacion: unaCalificacion

    "Métodos"
    initialize [unNombre unaEdad unaCalificacion]
        "Inicializa la instancia con los valores especificados."
        Persona initialize: unNombre edad: unaEdad.
        calificacion := unaCalificacion.

    saludo []
        "Devuelve un saludo del estudiante que incluye su calificacion."
        ^ 'Hola, me llamo ' , self nombre , ' y tengo ' , self edad , ' años. Mi calificación es ' , self calificacion , '.'
]

"Definiendo la clase 'Profesor'"
Profesor >> Persona [
    "Propiedades"
    asignatura: ""

    "Constructor"
    nuevo [unNombre unaEdad unaAsignatura]
        "Crea una nueva instancia de 'Profesor' con los valores especificados."
        ^ self nuevo initialize: unNombre edad: unaEdad asignatura: unaAsignatura

    "Métodos"
    initialize [unNombre unaEdad unaAsignatura]
        "Inicializa la instancia con los valores especificados."
        Persona initialize: unNombre edad: unaEdad.
        asignatura := unaAsignatura.

    saludo []
        "Devuelve un saludo del profesor que incluye su asignatura."
        ^ 'Hola, me llamo ' , self nombre , ' y tengo ' , self edad , ' años. Soy profesor de ' , self asignatura , '.'
]

"Definiendo la clase 'Director'"
Director >> Profesor [
    "Propiedades"
    escuela: ""

    "Constructor"
    nuevo [unNombre unaEdad unaAsignatura unaEscuela]
        "Crea una nueva instancia de 'Director' con los valores especificados."
        ^ self nuevo initialize: unNombre edad: unaEdad asignatura: unaAsignatura escuela: unaEscuela

    "Métodos"
    initialize [unNombre unaEdad unaAsignatura unaEscuela]
        "Inicializa la instancia con los valores especificados."
        Profesor initialize: unNombre edad: unaEdad asignatura: unaAsignatura.
        escuela := unaEscuela.

    saludo []
        "Devuelve un saludo del director que incluye su escuela."
        ^ 'Hola, me llamo ' , self nombre , ' y tengo ' , self edad , ' años. Soy director de ' , self escuela , '.'
]

"Creando un estudiante y un director"
unEstudiante := Estudiante nuevo: 'Juan' edad: 20 calificacion: 9.0.
unDirector := Director nuevo: 'Maria' edad: 45 asignatura: 'Matemáticas' escuela: 'Colegio Central'.

"Imprimiendo los saludos del estudiante y el director"
Transcript show: unEstudiante saludo; cr.
Transcript show: unDirector saludo.
```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante`, `Profesor` y `Director` que heredan de la clase `Persona`.
* La clase `Persona` tiene las propiedades `nombre` y `edad`, y los métodos `nuevo` para crear una nueva instancia, `initialize` para inicializar la instancia con los valores especificados, y `saludo` para obtener un saludo de la persona.
* La clase `Estudiante` hereda de la clase `Persona` y tiene la propiedad adicional `calificacion`. También tiene un método `saludo` que incluye la calificación del estudiante.
* La clase `Profesor` hereda de la clase `Persona` y tiene la propiedad adicional `asignatura`. También tiene un método `saludo` que incluye la asignatura del profesor.
* La clase `Director` hereda de la clase `Profesor` y tiene la propiedad adicional `escuela`. También tiene un método `saludo` que incluye la escuela del director.
* Se crean dos objetos, un estudiante y un director, y se imprimen sus saludos.