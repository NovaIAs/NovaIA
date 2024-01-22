```smalltalk
// Definimos una clase llamada "Estudiante"
Estudiante := Class new.

// Agregamos atributos a la clase "Estudiante"
Estudiante instVarNames: #('nombre' 'calificaciones').

// Definimos métodos para la clase "Estudiante"
Estudiante methodsFor: 'construcción' do: [ :category |
  category
    add: [ :newNombre :nuevasCalificaciones ]
    with: [ :unEstudiante |
        unEstudiante "inicializar" newNombre nuevasCalificaciones ]
].

Estudiante methodsFor: 'inicialización' do: [ :category |
  category
    add: [ :newNombre :nuevasCalificaciones ]
    with: [ :unEstudiante |
        unEstudiante nombre: newNombre.
        unEstudiante calificaciones: nuevasCalificaciones ]
].

Estudiante methodsFor: 'consulta' do: [ :category |
  category
    add: [ :unNombre ]
    with: [ :unEstudiante |
        unEstudiante nombre sí: [ :nombre |
            ^ nombre = unNombre ] ]
].

Estudiante methodsFor: 'información' do: [ :category |
  category
    add: [ ]
    with: [ :unEstudiante |
        unEstudiante printNombre.
        unEstudiante printCalificaciones ]
].

Estudiante methodsFor: 'privado' do: [ :category |
  category
    add: [ ]
    with: [ :unEstudiante |
        unEstudiante printNombre.
        unEstudiante printCalificaciones ]
].

// Definimos una clase llamada "Calificación"
Calificación := Class new.

// Agregamos atributos a la clase "Calificación"
Calificación instVarNames: #('nombre' 'valor').

// Definimos métodos para la clase "Calificación"
Calificación methodsFor: 'construcción' do: [ :category |
  category
    add: [ :newNombre :nuevoValor ]
    with: [ :unaCalificación |
        unaCalificación "inicializar" newNombre nuevoValor ]
].

Calificación methodsFor: 'inicialización' do: [ :category |
  category
    add: [ :newNombre :nuevoValor ]
    with: [ :unaCalificación |
        unaCalificación nombre: newNombre.
        unaCalificación valor: nuevoValor ]
].

Calificación methodsFor: 'consulta' do: [ :category |
  category
    add: [ ]
    with: [ :unaCalificación |
        unaCalificación printNombre ]
].

Calificación methodsFor: 'información' do: [ :category |
  category
    add: [ ]
    with: [ :unaCalificación |
        unaCalificación printNombre.
        unaCalificación printValor ]
].

Calificación methodsFor: 'privado' do: [ :category |
  category
    add: [ ]
    with: [ :unaCalificación |
        unaCalificación printNombre.
        unaCalificación printValor ]
].

// Creamos un estudiante llamado "Juan"
juan := Estudiante new nombre: 'Juan' calificaciones: [].

// Agregamos calificaciones al estudiante "Juan"
juan calificaciones add: Calificación new nombre: 'Matemáticas' valor: 90.
juan calificaciones add: Calificación new nombre: 'Ciencias' valor: 85.
juan calificaciones add: Calificación new nombre: 'Inglés' valor: 95.

// Imprimimos el nombre y las calificaciones del estudiante "Juan"
juan printNombre.
juan printCalificaciones.

```

Explicación del código:

1. Definimos dos clases, "Estudiante" y "Calificación", que representan a los estudiantes y a las calificaciones, respectivamente.
2. Añadimos atributos a cada clase, que son las propiedades que tendrán los objetos de esa clase. Por ejemplo, la clase "Estudiante" tiene los atributos "nombre" y "calificaciones".
3. Definimos métodos para cada clase, que son las acciones que los objetos de esa clase pueden realizar. Por ejemplo, la clase "Estudiante" tiene métodos para construir un nuevo estudiante, inicializar un estudiante con un nombre y unas calificaciones, consultar si un estudiante tiene un determinado nombre, y obtener información sobre un estudiante.
4. Creamos un estudiante llamado "Juan" utilizando el método "new" de la clase "Estudiante".
5. Añadimos calificaciones al estudiante "Juan" utilizando el método "add" de la clase "Estudiante".
6. Imprimimos el nombre y las calificaciones del estudiante "Juan" utilizando los métodos "printNombre" y "printCalificaciones" de la clase "Estudiante".