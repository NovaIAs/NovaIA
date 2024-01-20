```smalltalk
#!/usr/bin/env st

"Cargar las librerías necesarias"
Object subclass: #Persona.
Object subclass: #Estudiante.
Object subclass: #Profesor.
Object subclass: #Curso.

"Crear una jerarquía de clases"
Persona>>#nombre: [nombre] := nombre.
Persona>>#edad: [edad] := edad.
Persona>>#sexo: [sexo] := sexo.
Persona>>#toString := [ ^nombre , ', ', edad, ', ', sexo ].

Estudiante>>#universidad: [universidad] := universidad.
Estudiante>>#carrera: [carrera] := carrera.
Estudiante>>#toString := [ ^super toString , ', ', universidad , ', ', carrera ].

Profesor>>#asignatura: [asignatura] := asignatura.
Profesor>>#toString := [ ^super toString , ', ', asignatura ].

Curso>>#nombre: [nombre] := nombre.
Curso>>#profesor: [profesor] := profesor.
Curso>>#alumnos: [alumnos] := alumnos.
Curso>>#toString := [ ^nombre , ', ', profesor , ', ', alumnos ].

"Crear objetos"
persona1 := Persona new nombre: 'Juan' edad: 20 sexo: 'Masculino'.
persona2 := Estudiante new nombre: 'María' edad: 22 sexo: 'Femenino'.
persona3 := Profesor new nombre: 'Pedro' edad: 40 sexo: 'Masculino'.
curso := Curso new nombre: 'Introducción a la Programación' profesor: persona3 alumnos: #(persona1 persona2).

"Imprimir los objetos"
Transcript show: persona1 toString.
Transcript show: persona2 toString.
Transcript show: persona3 toString.
Transcript show: curso toString.
```

Este código crea una jerarquía de clases en Smalltalk, que incluye las clases Persona, Estudiante, Profesor y Curso. Luego, crea objetos de estas clases y los imprime en la consola.

La clase Persona tiene tres atributos: nombre, edad y sexo. La clase Estudiante hereda de la clase Persona y añade dos atributos más: universidad y carrera. La clase Profesor también hereda de la clase Persona y añade un atributo más: asignatura. La clase Curso tiene tres atributos: nombre, profesor y alumnos.

Los métodos `nombre:`, `edad:` y `sexo:` de la clase Persona establecen los valores de los atributos correspondientes. El método `toString` de la clase Persona devuelve una cadena de caracteres con los valores de los atributos.

Los métodos `universidad:` y `carrera:` de la clase Estudiante establecen los valores de los atributos correspondientes. El método `toString` de la clase Estudiante devuelve una cadena de caracteres con los valores de los atributos de la clase Persona y de la clase Estudiante.

El método `asignatura:` de la clase Profesor establece el valor del atributo correspondiente. El método `toString` de la clase Profesor devuelve una cadena de caracteres con los valores de los atributos de la clase Persona y de la clase Profesor.

Los métodos `nombre:`, `profesor:` y `alumnos:` de la clase Curso establecen los valores de los atributos correspondientes. El método `toString` de la clase Curso devuelve una cadena de caracteres con los valores de los atributos.

Los objetos `persona1`, `persona2` y `persona3` son instancias de las clases Persona, Estudiante y Profesor, respectivamente. El objeto `curso` es una instancia de la clase Curso.

El método `show:` de la clase Transcript muestra una cadena de caracteres en la consola. El método `toString` de los objetos `persona1`, `persona2`, `persona3` y `curso` devuelve una cadena de caracteres con los valores de sus atributos.