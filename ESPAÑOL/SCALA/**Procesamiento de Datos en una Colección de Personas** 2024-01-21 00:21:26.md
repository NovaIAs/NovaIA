```scala
// Creamos una lista de objetos `Persona`
val personas = List(
  Persona("Juan", 25),
  Persona("María", 28),
  Persona("Pedro", 32)
)

// Definimos una función para obtener la edad promedio de una lista de personas
def edadPromedio(personas: List[Persona]): Double = {
  // Sumamos todas las edades de las personas
  val sumaEdades = personas.map(_.edad).sum

  // Dividimos la suma de las edades entre el número de personas
  sumaEdades / personas.size
}

// Imprimimos la edad promedio de la lista de personas
println(edadPromedio(personas))

// Definimos una clase `Persona`
case class Persona(nombre: String, edad: Int)

// Definimos una clase `Compania`
case class Compania(nombre: String, empleados: List[Persona])

// Creamos una lista de objetos `Compania`
val companias = List(
  Compania("Google", List(
    Persona("Juan", 25),
    Persona("María", 28)
  )),
  Compania("Microsoft", List(
    Persona("Pedro", 32),
    Persona("Ana", 23)
  ))
)

// Definimos una función para obtener la edad promedio de los empleados de una compañía
def edadPromedioEmpleados(compania: Compania): Double = {
  // Obtenemos la lista de empleados de la compañía
  val empleados = compania.empleados

  // Calculamos la edad promedio de los empleados
  edadPromedio(empleados)
}

// Imprimimos la edad promedio de los empleados de cada compañía
companias.foreach(compania => {
  println(s"Edad promedio de los empleados de ${compania.nombre}: ${edadPromedioEmpleados(compania)}")
})

// Definimos una función para obtener la edad máxima de una lista de personas
def edadMaxima(personas: List[Persona]): Int = {
  // Obtenemos la edad máxima de las personas
  personas.maxBy(_.edad).edad
}

// Imprimimos la edad máxima de la lista de personas
println(edadMaxima(personas))

// Definimos una función para obtener la edad mínima de una lista de personas
def edadMinima(personas: List[Persona]): Int = {
  // Obtenemos la edad mínima de las personas
  personas.minBy(_.edad).edad
}

// Imprimimos la edad mínima de la lista de personas
println(edadMinima(personas))
```

**Explicación del código:**

El código anterior es una demostración de cómo crear y utilizar funciones y clases en Scala para realizar operaciones en colecciones de objetos.

1. Definimos una lista de objetos `Persona` que contiene información sobre el nombre y la edad de cada persona.

2. Definimos una función `edadPromedio` que calcula la edad promedio de una lista de personas. La función suma todas las edades de las personas y divide la suma entre el número de personas.

3. Imprimimos la edad promedio de la lista de personas.

4. Definimos una clase `Persona` que representa a una persona con su nombre y edad.

5. Definimos una clase `Compania` que representa a una compañía con su nombre y una lista de empleados (que son objetos `Persona`).

6. Creamos una lista de objetos `Compania` que contiene información sobre el nombre de la compañía y la lista de empleados.

7. Definimos una función `edadPromedioEmpleados` que calcula la edad promedio de los empleados de una compañía. La función obtiene la lista de empleados de la compañía y calcula la edad promedio de los empleados utilizando la función `edadPromedio`.

8. Imprimimos la edad promedio de los empleados de cada compañía.

9. Definimos una función `edadMaxima` que calcula la edad máxima de una lista de personas. La función obtiene la edad máxima de las personas utilizando la función `maxBy`.

10. Imprimimos la edad máxima de la lista de personas.

11. Definimos una función `edadMinima` que calcula la edad mínima de una lista de personas. La función obtiene la edad mínima de las personas utilizando la función `minBy`.

12. Imprimimos la edad mínima de la lista de personas.

Este código es un ejemplo de cómo usar Scala para crear y manipular objetos y colecciones de objetos. También demuestra el uso de funciones y clases para realizar operaciones complejas en los datos.