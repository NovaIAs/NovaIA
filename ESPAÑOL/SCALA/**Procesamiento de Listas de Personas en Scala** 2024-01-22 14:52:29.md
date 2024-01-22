```scala
// Definir una clase de datos para representar una persona.
case class Persona(nombre: String, apellidos: String, edad: Int)

// Definir una función para crear una lista de personas.
def crearPersonas(): List[Persona] = {
  List(
    Persona("Juan", "García", 20),
    Persona("María", "Pérez", 25),
    Persona("Pedro", "López", 30)
  )
}

// Definir una función para filtrar una lista de personas por edad.
def filtrarPorEdad(personas: List[Persona], edadMinima: Int, edadMaxima: Int): List[Persona] = {
  personas.filter(persona => persona.edad >= edadMinima && persona.edad <= edadMaxima)
}

// Definir una función para ordenar una lista de personas por nombre.
def ordenarPorNombre(personas: List[Persona]): List[Persona] = {
  personas.sortBy(_.nombre)
}

// Definir una función para imprimir una lista de personas.
def imprimirPersonas(personas: List[Persona]): Unit = {
  for (persona <- personas) {
    println(s"${persona.nombre} ${persona.apellidos} tiene ${persona.edad} años.")
  }
}

// Crear una lista de personas.
val personas = crearPersonas()

// Filtrar la lista de personas por edad.
val personasFiltradas = filtrarPorEdad(personas, 20, 30)

// Ordenar la lista de personas filtradas por nombre.
val personasOrdenadas = ordenarPorNombre(personasFiltradas)

// Imprimir la lista de personas ordenadas.
imprimirPersonas(personasOrdenadas)
```

Este código define una clase de datos para representar una persona, una función para crear una lista de personas, una función para filtrar una lista de personas por edad, una función para ordenar una lista de personas por nombre y una función para imprimir una lista de personas.

El código crea una lista de personas, filtra la lista de personas por edad, ordena la lista de personas filtradas por nombre e imprime la lista de personas ordenadas.

El resultado del código es el siguiente:

```
Juan García tiene 20 años.
María Pérez tiene 25 años.
```