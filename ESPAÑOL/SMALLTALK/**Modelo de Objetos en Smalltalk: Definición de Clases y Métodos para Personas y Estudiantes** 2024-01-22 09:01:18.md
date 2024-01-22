```smalltalk
"Definir una clase llamada 'Persona'"
Persona := Clase nombre: 'Persona'.

"Añadir una variable de instancia llamada 'nombre'"
Persona instVars: ['nombre'].

"Definir un método llamado 'initialize' para inicializar la instancia"
Persona class methods: [
	initialize: unNombre [
		super initialize. "Llama al método initialize de la clase padre"
		nombre := unNombre. "Establece el nombre de la persona"
	]
].

"Definir un método llamado 'nombre' para obtener el nombre de la persona"
Persona methods: [
	nombre [
		^nombre "Devuelve el nombre de la persona"
	]
].

"Definir una clase llamada 'Estudiante' que hereda de 'Persona'"
Estudiante := Persona subclass: 'Estudiante'.

"Añadir una variable de instancia llamada 'calificaciones'"
Estudiante instVars: ['calificaciones'].

"Definir un método llamado 'initialize' para inicializar la instancia"
Estudiante class methods: [
	initialize: unNombre [
		super initialize: unNombre. "Llama al método initialize de la clase padre"
		calificaciones := OrderedCollection new. "Crea una nueva colección ordenada para las calificaciones"
	]
].

"Definir un método llamado 'agregarCalificacion' para agregar una calificación a la colección"
Estudiante methods: [
	agregarCalificacion: unaCalificacion [
		calificaciones add: unaCalificacion. "Agrega la calificación a la colección"
	]
].

"Definir un método llamado 'promedio' para calcular el promedio de las calificaciones"
Estudiante methods: [
	promedio [
		^calificaciones sum / calificaciones size. "Calcula el promedio de las calificaciones"
	]
].

"Crear una instancia de la clase 'Estudiante'"
unEstudiante := Estudiante initialize: 'Juan'. "Crea un nuevo estudiante llamado 'Juan'"

"Agregar algunas calificaciones al estudiante"
unEstudiante agregarCalificacion: 90.
unEstudiante agregarCalificacion: 85.
unEstudiante agregarCalificacion: 95.

"Mostrar el nombre y el promedio del estudiante en la consola"
Transcript show: 'Nombre: ', unEstudiante nombre, cr.
Transcript show: 'Promedio: ', unEstudiante promedio, cr.
```

Explicación:

* Definimos la clase `Persona` con una variable de instancia llamada `nombre`.
* Definimos la clase `Estudiante` que hereda de `Persona` y añadimos una variable de instancia llamada `calificaciones`.
* Definimos métodos para inicializar las instancias de las clases, obtener el nombre de una persona, agregar calificaciones a un estudiante y calcular el promedio de las calificaciones.
* Creamos una instancia de la clase `Estudiante` llamada `unEstudiante` y añadimos algunas calificaciones.
* Mostramos el nombre y el promedio del estudiante en la consola.