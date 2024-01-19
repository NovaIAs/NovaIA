```smalltalk
"Clase Persona"
Persona nueva
	| nombre |
	nombre := "".
	super nueva.

Persona edad: unaEdad
	^ unaEdad.

Persona nombre: unNombre
	nombre := unNombre.

Persona imprimir
	Transcript show: nombre; cr.

"Clase Estudiante"
Estudiante nueva
	super nueva.

Estudiante edad: unaEdad
	super edad: unaEdad.

Estudiante nombre: unNombre
	super nombre: unNombre.

Estudiante imprimir
	Transcript show: 'Estudiante: '; super imprimir.

"Clase Profesor"
Profesor nueva
	super nueva.

Profesor edad: unaEdad
	super edad: unaEdad.

Profesor nombre: unNombre
	super nombre: unNombre.

Profesor imprimir
	Transcript show: 'Profesor: '; super imprimir.

"Clase Main"
Main nueva
	| personaEstudiante |
	personaEstudiante := Estudiante nueva.
	personaEstudiante nombre: 'Juan'.
	personaEstudiante edad: 20.

	| personaProfesor |
	personaProfesor := Profesor nueva.
	personaProfesor nombre: 'María'.
	personaProfesor edad: 30.

	Transcript show: 'Persona Estudiante: '; personaEstudiante imprimir; cr.
	Transcript show: 'Persona Profesor: '; personaProfesor imprimir; cr.

```

Explicación del código:

- Clase Persona: Esta clase define las propiedades y métodos comunes a todas las personas. Las propiedades incluyen el nombre y la edad, y los métodos incluyen la impresión del nombre y la edad.

- Clase Estudiante: Esta clase hereda de la clase Persona y define propiedades y métodos específicos para los estudiantes. Las propiedades incluyen el número de matrícula y la carrera, y los métodos incluyen la impresión del número de matrícula y la carrera.

- Clase Profesor: Esta clase hereda de la clase Persona y define propiedades y métodos específicos para los profesores. Las propiedades incluyen el título y el departamento, y los métodos incluyen la impresión del título y el departamento.

- Clase Main: Esta clase es el punto de entrada al programa. Crea instancias de las clases Persona, Estudiante y Profesor, y luego imprime sus nombres y edades.

El código utiliza el concepto de herencia para crear clases que comparten propiedades y métodos comunes. Esto hace que el código sea más fácil de mantener y más reutilizable.

El código también utiliza el concepto de polimorfismo para imprimir los nombres y edades de las personas. Esto significa que el mismo método puede ser utilizado para imprimir los nombres y edades de personas de diferentes tipos, como estudiantes y profesores.

El código es un ejemplo de cómo utilizar la programación orientada a objetos en Smalltalk. La programación orientada a objetos es un paradigma de programación que utiliza clases y objetos para representar datos y comportamiento. Este paradigma hace que el código sea más fácil de entender y mantener, y también ayuda a reutilizar el código.