```smalltalk
**Clase Persona**

Persona clase [
   nombre: ''
   edad: 0
   género: ''
]

**Método para obtener el nombre de la persona**

Persona obtenerNombre [] [
   ^nombre
]

**Método para establecer el nombre de la persona**

Persona establecerNombre [nuevoNombre] [
   nombre:= nuevoNombre
]

**Método para obtener la edad de la persona**

Persona obtenerEdad [] [
   ^edad
]

**Método para establecer la edad de la persona**

Persona establecerEdad [nuevaEdad] [
   edad:= nuevaEdad
]

**Método para obtener el género de la persona**

Persona obtenerGénero [] [
   ^género
]

**Método para establecer el género de la persona**

Persona establecerGénero [nuevoGénero] [
   género:= nuevoGénero
]

**Método para imprimir los datos de la persona**

Persona imprimir [] [
   Transcript show: 'Nombre: ', nombre, '. ';
   Transcript show: 'Edad: ', edad, '. ';
   Transcript show: 'Género: ', género, '. ';
   Transcript cr
]

**Creación de un objeto Persona**

persona1:= Persona crear.

**Establecimiento de los datos de la persona**

persona1 establecerNombre: 'Juan'.
persona1 establecerEdad: 25.
persona1 establecerGénero: 'Masculino'.

**Impresión de los datos de la persona**

persona1 imprimir.

**Explicando el Código:**

1. Creamos la clase Persona con tres atributos: nombre, edad y género.
2. Definimos métodos para obtener y establecer cada uno de los atributos.
3. También definimos un método para imprimir los datos de la persona.
4. Creamos un objeto Persona llamado persona1.
5. Establecemos los datos de la persona1.
6. Imprimimos los datos de la persona1.

Este código muestra cómo crear una clase y un objeto en Smalltalk, así como cómo definir métodos para obtener y establecer los atributos del objeto.