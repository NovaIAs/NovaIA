```scala
// Definición de una clase Persona con propiedades nombre, edad y sexo.
class Persona(val nombre: String, val edad: Int, val sexo: Char) {

  // Método para saludar.
  def saludar(): Unit = {
    println(s"Hola, soy $nombre y tengo $edad años.")
  }

  // Método para obtener el sexo de la persona.
  def getSexo(): Char = {
    sexo
  }
}

// Definición de una clase Estudiante que hereda de la clase Persona.
class Estudiante(nombre: String, edad: Int, sexo: Char, val matricula: Int) extends Persona(nombre, edad, sexo) {

  // Método para estudiar.
  def estudiar(): Unit = {
    println(s"Estoy estudiando.")
  }

  // Método para obtener la matrícula del estudiante.
  def getMatricula(): Int = {
    matricula
  }
}

// Definición de una clase Profesor que hereda de la clase Persona.
class Profesor(nombre: String, edad: Int, sexo: Char, val asignatura: String) extends Persona(nombre, edad, sexo) {

  // Método para enseñar.
  def enseñar(): Unit = {
    println(s"Estoy enseñando.")
  }

  // Método para obtener la asignatura del profesor.
  def getAsignatura(): String = {
    asignatura
  }
}

// Definición de una clase Universidad que contiene una lista de estudiantes y profesores.
class Universidad(val estudiantes: List[Estudiante], val profesores: List[Profesor]) {

  // Método para añadir un estudiante a la universidad.
  def añadirEstudiante(estudiante: Estudiante): Unit = {
    estudiantes :+ estudiante
  }

  // Método para añadir un profesor a la universidad.
  def añadirProfesor(profesor: Profesor): Unit = {
    profesores :+ profesor
  }

  // Método para obtener la lista de estudiantes de la universidad.
  def getEstudiantes(): List[Estudiante] = {
    estudiantes
  }

  // Método para obtener la lista de profesores de la universidad.
  def getProfesores(): List[Profesor] = {
    profesores
  }
}

// Función principal.
def main(args: Array[String]): Unit = {

  // Creación de una universidad.
  val universidad = new Universidad(List(), List())

  // Creación de un estudiante.
  val estudiante = new Estudiante("Juan", 20, 'M', 12345)

  // Creación de un profesor.
  val profesor = new Profesor("María", 30, 'F', "Matemáticas")

  // Añadir el estudiante y el profesor a la universidad.
  universidad.añadirEstudiante(estudiante)
  universidad.añadirProfesor(profesor)

  // Obtener la lista de estudiantes y profesores de la universidad.
  val estudiantes = universidad.getEstudiantes()
  val profesores = universidad.getProfesores()

  // Imprimir la información de los estudiantes y profesores.
  println("Estudiantes:")
  for (estudiante <- estudiantes) {
    println(s"${estudiante.nombre} - ${estudiante.edad} - ${estudiante.sexo} - ${estudiante.matricula}")
  }

  println("Profesores:")
  for (profesor <- profesores) {
    println(s"${profesor.nombre} - ${profesor.edad} - ${profesor.sexo} - ${profesor.asignatura}")
  }
}
```

Explicación del código:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es la clase base, y las clases `Estudiante` y `Profesor` heredan de ella.
* La clase `Persona` tiene tres propiedades: `nombre`, `edad` y `sexo`. También tiene un método para saludar.
* La clase `Estudiante` hereda las propiedades y el método de la clase `Persona`, y añade una propiedad `matricula` y un método para estudiar.
* La clase `Profesor` hereda las propiedades y el método de la clase `Persona`, y añade una propiedad `asignatura` y un método para enseñar.
* Se define una clase `Universidad` que contiene una lista de estudiantes y profesores. La clase `Universidad` tiene métodos para añadir estudiantes y profesores, y para obtener la lista de estudiantes y profesores.
* En la función `main`, se crea una instancia de la clase `Universidad`, se crean un estudiante y un profesor, y se añaden a la universidad. Luego, se obtiene la lista de estudiantes y profesores de la universidad y se imprime en la consola.