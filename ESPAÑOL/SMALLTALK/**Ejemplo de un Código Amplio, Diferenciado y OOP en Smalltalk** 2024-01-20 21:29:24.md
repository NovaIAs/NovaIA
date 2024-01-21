```smalltalk
"Ejemplo de un código muy amplio y diferenciado en Smalltalk"

"Clase Persona"
Persona clase | nombre edad |
  propiedades: [ nombre : String | edad : Número ]

  "Crea una nueva persona con el nombre y la edad dados."
  nuevo: unNombre unaEdad ->
    [ nombre := unNombre | edad := unaEdad |
      self ]

  "Devuelve el nombre de la persona."
  obtenerNombre ->
    nombre

  "Devuelve la edad de la persona."
  obtenerEdad ->
    edad

  "Aumenta la edad de la persona en un año."
  cumpleaños ->
    [ edad := edad + 1 ]

  "imprime el nombre y la edad de la persona."
  imprimir ->
    [ Transcript show: nombre; show: ', '; show: edad; cr ]

"Clase Estudiante"
Estudiante clase | nombre edad carrera |
  propiedades: [ nombre : String | edad : Número | carrera : String ]

  "Crea un nuevo estudiante con el nombre, la edad y la carrera dados."
  nuevo: unNombre unaEdad unaCarrera ->
    [ nombre := unNombre | edad := unaEdad | carrera := unaCarrera |
      super nuevo: unNombre unaEdad ]

  "Devuelve la carrera del estudiante."
  obtenerCarrera ->
    carrera

  "imprime el nombre, la edad y la carrera del estudiante."
  imprimir ->
    [ super imprimir | Transcript show: ', '; show: carrera; cr ]

"Clase Profesor"
Profesor clase | nombre edad materia |
  propiedades: [ nombre : String | edad : Número | materia : String ]

  "Crea un nuevo profesor con el nombre, la edad y la materia dados."
  nuevo: unNombre unaEdad unaMateria ->
    [ nombre := unNombre | edad := unaEdad | materia := unaMateria |
      super nuevo: unNombre unaEdad ]

  "Devuelve la materia del profesor."
  obtenerMateria ->
    materia

  "imprime el nombre, la edad y la materia del profesor."
  imprimir ->
    [ super imprimir | Transcript show: ', '; show: materia; cr ]

"Ejemplo de uso de las clases"
persona1 := Persona nueva: 'Juan' 20
persona2 := Estudiante nueva: 'María' 21 'Ingeniería'
persona3 := Profesor nueva: 'Pedro' 30 'Matemáticas'

"Imprime los datos de las personas"
persona1 imprimir
persona2 imprimir
persona3 imprimir
```

Explicación:

* La clase `Persona` representa a una persona con propiedades de nombre y edad.
* La clase `Estudiante` es una subclase de `Persona` que representa a un estudiante con propiedades de carrera.
* La clase `Profesor` es una subclase de `Persona` que representa a un profesor con propiedades de materia.
* El ejemplo de uso crea tres objetos: una persona, un estudiante y un profesor, y luego imprime sus datos.

Este código es muy amplio y diferenciado porque:

* Utiliza herencia para crear clases que comparten características comunes.
* Utiliza polimorfismo para enviar mensajes a objetos de diferentes clases que responden de manera diferente.
* Utiliza encapsulamiento para ocultar los detalles de implementación de las clases.
* Utiliza abstracción para representar objetos del mundo real en un formato que es fácil de entender y manipular.