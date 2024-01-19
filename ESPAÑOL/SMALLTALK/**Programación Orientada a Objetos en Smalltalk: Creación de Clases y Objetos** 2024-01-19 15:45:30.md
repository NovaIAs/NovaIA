```smalltalk
"Definir una clase llamada 'Persona' con atributos 'nombre', 'apellido' y 'edad'."
Object subclass: Persona
	instanceVariableNames: 'nombre apellido edad'

"Definir un método llamado 'initialize' para inicializar los atributos de la clase 'Persona'."
Persona class method initialize: nombre apellido edad
	self new
		nombre: nombre;
		apellido: apellido;
		edad: edad

"Definir un método llamado 'nombre' para obtener el nombre de una persona."
Persona method nombre
	nombre

"Definir un método llamado 'apellido' para obtener el apellido de una persona."
Persona method apellido
	apellido

"Definir un método llamado 'edad' para obtener la edad de una persona."
Persona method edad
	edad

"Definir una clase llamada 'Estudiante' que herede de la clase 'Persona'."
Persona subclass: Estudiante
	instanceVariableNames: 'matricula'

"Definir un método llamado 'initialize' para inicializar los atributos de la clase 'Estudiante'."
Estudiante class method initialize: nombre apellido edad matricula
	self new
		super initialize: nombre apellido edad;
		matricula: matricula

"Definir un método llamado 'matricula' para obtener la matrícula de un estudiante."
Estudiante method matricula
	matricula

"Crear una instancia de la clase 'Persona' llamada 'juan'."
juan := Persona new initialize: 'Juan' 'Pérez' 25

"Crear una instancia de la clase 'Estudiante' llamada 'maría'."
maría := Estudiante new initialize: 'María' 'García' 20 '12345'

"Imprimir el nombre, apellido y edad de 'juan'."
Transcript show: juan nombre; show: ' '; show: juan apellido; show: ' '; show: juan edad; cr

"Imprimir el nombre, apellido, edad y matrícula de 'maría'."
Transcript show: maría nombre; show: ' '; show: maría apellido; show: ' '; show: maría edad; show: ' '; show: maría matricula; cr
```

Explicación:

* Se define una clase llamada 'Persona' con atributos 'nombre', 'apellido' y 'edad'.
* Se define un método llamado 'initialize' para inicializar los atributos de la clase 'Persona'.
* Se definen métodos llamados 'nombre', 'apellido' y 'edad' para obtener los valores de los atributos correspondientes.
* Se define una clase llamada 'Estudiante' que hereda de la clase 'Persona'.
* Se define un atributo llamado 'matricula' para la clase 'Estudiante'.
* Se define un método llamado 'initialize' para inicializar los atributos de la clase 'Estudiante'.
* Se define un método llamado 'matricula' para obtener el valor del atributo 'matricula'.
* Se crean instancias de las clases 'Persona' y 'Estudiante' llamadas 'juan' y 'maría', respectivamente.
* Se imprimen el nombre, apellido y edad de 'juan', y el nombre, apellido, edad y matrícula de 'maría'.