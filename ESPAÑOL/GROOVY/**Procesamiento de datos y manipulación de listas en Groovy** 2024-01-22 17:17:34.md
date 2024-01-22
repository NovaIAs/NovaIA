```groovy
// Definir una lista de nombres
def nombres = ["Pedro", "Juan", "María", "José", "Ana"]

// Crear una función para saludar a una persona
def saludar(nombre) {
  "Hola, $nombre!"
}

// Utilizar la función map() para aplicar la función saludar a cada nombre de la lista
def saludos = nombres.map(saludar)

// Mostrar los saludos en la consola
println saludos

// Definir un mapa de nombres y edades
def edades = [Pedro: 25, Juan: 30, María: 35, José: 40, Ana: 45]

// Crear una función para obtener la edad de una persona
def obtenerEdad(nombre) {
  edades[nombre]
}

// Utilizar la función find() para encontrar el nombre de la persona más joven
def personaMasJoven = nombres.find { obtenerEdad(it) == edades.values().min() }

// Mostrar el nombre de la persona más joven en la consola
println "La persona más joven es $personaMasJoven"

// Crear una función para calcular el promedio de edad de un grupo de personas
def calcularPromedioEdad(nombres) {
  def sumaEdades = 0
  nombres.each { sumaEdades += obtenerEdad(it) }
  sumaEdades / nombres.size()
}

// Calcular el promedio de edad de las personas en la lista
def promedioEdad = calcularPromedioEdad(nombres)

// Mostrar el promedio de edad en la consola
println "El promedio de edad es $promedioEdad"
```

Este código realiza las siguientes tareas:

* Define una lista de nombres.
* Crea una función para saludar a una persona.
* Utiliza la función `map()` para aplicar la función `saludar()` a cada nombre de la lista.
* Muestra los saludos en la consola.
* Define un mapa de nombres y edades.
* Crea una función para obtener la edad de una persona.
* Utiliza la función `find()` para encontrar el nombre de la persona más joven.
* Muestra el nombre de la persona más joven en la consola.
* Crea una función para calcular el promedio de edad de un grupo de personas.
* Calcula el promedio de edad de las personas en la lista.
* Muestra el promedio de edad en la consola.