```smalltalk
"Definir la clase 'Persona'"
Persona := Class nombre: 'Persona'.

"Definir atributos de la clase 'Persona'"
Persona instVarNames: 'nombre edad'.

"Definir métodos de la clase 'Persona'"
Persona agregaMétodo: [:unaPersona] [
  "Obtener el nombre de la persona"
  ^ unaPersona nombre asString.
].

"Definir una subclase de 'Persona' llamada 'Profesor'"
Profesor := Persona subclass: #Profesor nombre: 'Profesor'.

"Definir atributos de la clase 'Profesor'"
Profesor instVarNames: 'materia'.

"Definir métodos de la clase 'Profesor'"
Profesor agregaMétodo: [:unProfesor] [
  "Obtener la materia del profesor"
  ^ unProfesor materia asString.
].

"Crear una instancia de la clase 'Persona' llamada 'Carlos'"
carlos := Persona new nombre: 'Carlos' edad: 30.

"Crear una instancia de la clase 'Profesor' llamada 'María'"
maría := Profesor new nombre: 'María' edad: 35 materia: 'Matemáticas'.

"Imprimir el nombre de Carlos"
Transcript show: carlos nombre.

"Imprimir la materia de María"
Transcript show: maría materia.
```

Explicación:

* Creamos la clase `Persona` con los atributos `nombre` y `edad`.
* Creamos la subclase `Profesor` de `Persona` con el atributo `materia`.
* Creamos dos instancias de `Persona` y `Profesor`, llamadas `carlos` y `maría`, respectivamente.
* Imprimimos el nombre de `carlos` y la materia de `maría`.

Este código es un ejemplo de cómo crear clases y objetos en Smalltalk. También muestra cómo usar subclases para crear clases más especializadas.