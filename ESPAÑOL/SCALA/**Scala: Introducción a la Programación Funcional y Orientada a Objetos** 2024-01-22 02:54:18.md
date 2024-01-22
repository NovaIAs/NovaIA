```scala
// Definición de clases y objetos
class Persona(nombre: String, edad: Int) {
  def saludar(): Unit = {
    println(s"Hola, soy $nombre y tengo $edad años.")
  }
}

object Persona {
  def apply(nombre: String, edad: Int): Persona = new Persona(nombre, edad)
}

// Definición de funciones y métodos
def sumar(a: Int, b: Int): Int = a + b

// Creación de objetos
val persona1 = Persona("Juan", 25)
val persona2 = Persona("María", 30)

// Uso de funciones y métodos
val suma = sumar(1, 2)

// Uso de objetos
persona1.saludar()
persona2.saludar()

// Uso de funciones y métodos en objetos
val sumaPersona1y2 = persona1.sumar(persona2.edad)

// Uso de patrones y match
val resultado = sumaPersona1y2 match {
  case 55 => "La suma es igual a 55"
  case _ => "La suma no es igual a 55"
}

// Uso de listas y bucles
val lista = List(1, 2, 3, 4, 5)

lista.foreach(x => println(x))

// Uso de expresiones lambda
val listaFiltrada = lista.filter(_ > 2)

// Uso de recursividad
def factorial(n: Int): Int = {
  if (n == 0) {
    1
  } else {
    n * factorial(n-1)
  }
}

// Uso de objetos inmutables
val personaInmutable = persona1.copy(edad = 30)

// Uso de genéricos
class Lista[T](elementos: List[T]) {
  def primero: T = elementos.head
  def resto: Lista[T] = Lista(elementos.tail)
}

val listaDeEnteros = Lista(1, 2, 3, 4, 5)
val listaDeCadenas = Lista("Hola", "Mundo", "!")

// Uso de programación funcional
val listaDeCuadrados = listaDeEnteros.map(x => x * x)

// Uso de programación concurrente
val future = Future {
  Thread.sleep(1000)
  "Resultado futuro"
}

future.onComplete {
  case Success(resultado) => println(resultado)
  case Failure(excepcion) => println(excepcion.getMessage)
}

Thread.sleep(2000) // Espera a que el futuro se complete
```

Explicación del código:

* **Definición de clases y objetos:** En Scala, las clases y los objetos se definen utilizando la palabra clave `class` y `object`, respectivamente. En este ejemplo, se define una clase `Persona` y un objeto `Persona`. La clase `Persona` tiene dos campos, `nombre` y `edad`, y un método `saludar()`. El objeto `Persona` es un objeto compañero de la clase `Persona`, y se utiliza para crear nuevos objetos de tipo `Persona`.
* **Definición de funciones y métodos:** En Scala, las funciones y los métodos se definen utilizando la palabra clave `def`. En este ejemplo, se define una función `sumar()` que toma dos parámetros de tipo `Int` y devuelve un valor de tipo `Int`.
* **Creación de objetos:** En Scala, los objetos se crean utilizando la palabra clave `new`. En este ejemplo, se crean dos objetos de tipo `Persona`, llamados `persona1` y `persona2`.
* **Uso de funciones y métodos:** En Scala, las funciones y los métodos se llaman utilizando la sintaxis de punto. En este ejemplo, se llama al método `saludar()` de los objetos `persona1` y `persona2`.
* **Uso de funciones y métodos en objetos:** En Scala, las funciones y los métodos también se pueden utilizar en objetos. En este ejemplo, se llama al método `sumar()` del objeto `persona1` con el argumento `persona2.edad`.
* **Uso de patrones y match:** En Scala, los patrones y match se utilizan para comparar el valor de una expresión con un conjunto de patrones. En este ejemplo, se compara el valor de la variable `sumaPersona1y2` con los patrones `55` y `_`. Si el valor de `sumaPersona1y2` es igual a `55`, se devuelve el valor `"La suma es igual a 55"`, de lo contrario, se devuelve el valor `"La suma no es igual a 55"`.
* **Uso de listas y bucles:** En Scala, las listas son una colección de elementos ordenados. En este ejemplo, se crea una lista `lista` con los elementos `1`, `2`, `3`, `4` y `5`. Luego, se recorre la lista utilizando un bucle `foreach` y se imprime cada elemento en la consola.
* **Uso de expresiones lambda:** En Scala, las expresiones lambda son una forma concisa de definir funciones. En este ejemplo, se utiliza una expresión lambda para filtrar la lista `lista` y seleccionar solo los elementos mayores que `2`.
* **Uso de recursividad:** En Scala, la recursividad es una técnica que consiste en llamar a una función desde dentro de sí misma. En este ejemplo, se define una función `factorial()` que calcula el factorial de un número entero.
* **Uso de objetos inmutables:** En Scala, los objetos inmutables son objetos que no se pueden modificar. En este ejemplo, se crea un objeto inmutable `personaInmutable` que es una copia del objeto `persona1`, pero con la edad cambiada a `30`.
* **Uso de genéricos:** En Scala, los genéricos son una forma de definir tipos de datos que pueden contener cualquier tipo de valor. En este ejemplo, se define una clase `Lista` que puede contener cualquier tipo de valor. Luego, se crean dos listas, una con valores enteros y otra con valores de cadena.
* **Uso de programación funcional:** En Scala, la programación funcional es un paradigma de programación que se basa en el uso de funciones y expresiones lambda. En este ejemplo, se utiliza programación funcional para crear una lista de cuadrados de los elementos de la lista `listaDeEnteros`.
* **Uso de programación concurrente:** En Scala, la programación concurrente es una forma de escribir programas que pueden ejecutar tareas en paralelo. En este ejemplo, se utiliza programación concurrente para crear un futuro que calculará el resultado de una operación que tarda 1 segundo en completarse. Luego, se espera a que el futuro se complete y se imprime el resultado en la consola.