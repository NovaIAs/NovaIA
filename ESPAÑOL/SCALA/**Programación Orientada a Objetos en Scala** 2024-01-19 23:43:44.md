```scala
// Definición de clases
class Persona(nombre: String, edad: Int) {
  def saludar(): Unit = {
    println("Hola, mi nombre es " + nombre + " y tengo " + edad + " años.")
  }
}

class Estudiante(nombre: String, edad: Int, carrera: String) extends Persona(nombre, edad) {
  def estudiar(): Unit = {
    println("Estoy estudiando " + carrera + ".")
  }
}

class Profesor(nombre: String, edad: Int, asignatura: String) extends Persona(nombre, edad) {
  def enseñar(): Unit = {
    println("Estoy enseñando " + asignatura + ".")
  }
}

// Creación de objetos
val persona1 = new Persona("Juan", 20)
val estudiante1 = new Estudiante("María", 22, "Ingeniería Informática")
val profesor1 = new Profesor("Pedro", 30, "Matemáticas")

// Llamada a métodos
persona1.saludar()
estudiante1.saludar()
estudiante1.estudiar()
profesor1.saludar()
profesor1.enseñar()

// Uso de colecciones
val listaPersonas: List[Persona] = List(persona1, estudiante1, profesor1)

listaPersonas.foreach(_.saludar())

val listaEstudiantes: List[Estudiante] = listaPersonas.collect { case estudiante: Estudiante => estudiante }

listaEstudiantes.foreach(_.estudiar())

// Uso de opciones
val maybeEstudiante: Option[Estudiante] = listaPersonas.find(_.isInstanceOf[Estudiante])

maybeEstudiante match {
  case Some(estudiante) => estudiante.estudiar()
  case None => println("No hay estudiantes en la lista.")
}

// Uso de funciones de orden superior
val suma: (Int, Int) => Int = (x, y) => x + y

val listaNumeros: List[Int] = List(1, 2, 3, 4, 5)

val sumaTotal: Int = listaNumeros.foldLeft(0)(suma)

println("La suma total es: " + sumaTotal)

// Uso de clases abstractas y traits
trait SerVivo {
  def vivir(): Unit
}

class Animal extends SerVivo {
  override def vivir(): Unit = {
    println("Estoy viviendo.")
  }
}

class Planta extends SerVivo {
  override def vivir(): Unit = {
    println("Estoy creciendo.")
  }
}

val animal1 = new Animal()
val planta1 = new Planta()

animal1.vivir()
planta1.vivir()

// Uso de genéricos
class Caja[T](contenido: T) {
  def obtenerContenido(): T = {
    contenido
  }
}

val caja1 = new Caja("Hola mundo")
val caja2 = new Caja(10)

println(caja1.obtenerContenido())
println(caja2.obtenerContenido())
```

Explicación:

* **Definición de clases:** Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base, y las clases `Estudiante` y `Profesor` son clases derivadas.
* **Creación de objetos:** Se crean tres objetos: `persona1`, `estudiante1` y `profesor1`.
* **Llamada a métodos:** Se llaman a los métodos `saludar()`, `estudiar()` y `enseñar()` de los objetos creados.
* **Uso de colecciones:** Se crea una lista de objetos `Persona` llamada `listaPersonas`. Se utiliza el método `foreach()` para iterar sobre la lista y llamar al método `saludar()` de cada objeto. También se crea una lista de objetos `Estudiante` llamada `listaEstudiantes`. Se utiliza el método `collect()` para filtrar la lista `listaPersonas` y obtener solo los objetos `Estudiante`. Se utiliza el método `foreach()` para iterar sobre la lista `listaEstudiantes` y llamar al método `estudiar()` de cada objeto.
* **Uso de opciones:** Se crea una opción `maybeEstudiante` que contiene un objeto `Estudiante`. Se utiliza el método `match()` para comprobar si la opción contiene un valor. Si contiene un valor, se llama al método `estudiar()` del objeto `Estudiante`. Si no contiene un valor, se imprime un mensaje.
* **Uso de funciones de orden superior:** Se crea una función `suma` que suma dos números. Se utiliza el método `foldLeft()` para aplicar la función `suma` a la lista `listaNumeros`. El resultado de la aplicación es la suma total de los números de la lista.
* **Uso de clases abstractas y traits:** Se define un trait `SerVivo` que tiene un método abstracto `vivir()`. Se crean dos clases, `Animal` y `Planta`, que implementan el trait `SerVivo`. Se crean dos objetos, `animal1` y `planta1`, de las clases `Animal` y `Planta`, respectivamente. Se llaman a los métodos `vivir()` de los objetos creados.
* **Uso de genéricos:** Se define una clase `Caja` que tiene un parámetro de tipo `T`. Se crean dos objetos `Caja`, uno con un contenido de tipo `String` y otro con un contenido de tipo `Int`. Se llaman a los métodos `obtenerContenido()` de los objetos creados para obtener el contenido de las cajas.