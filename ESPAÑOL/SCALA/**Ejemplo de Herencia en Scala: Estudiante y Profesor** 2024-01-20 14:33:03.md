```scala
// Clase Persona
class Persona(nombre: String, edad: Int) {
    def saludar(): String = s"Hola, mi nombre es $nombre y tengo $edad años"
}

// Clase Estudiante que hereda de Persona
class Estudiante(nombre: String, edad: Int, curso: String) extends Persona(nombre, edad) {
    def estudiar(): String = s"Estoy estudiando $curso"
}

// Clase Profesor que hereda de Persona
class Profesor(nombre: String, edad: Int, asignatura: String) extends Persona(nombre, edad) {
    def enseñar(): String = s"Estoy enseñando $asignatura"
}

// Clase Main
object Main {
    def main(args: Array[String]): Unit = {
        // Creamos una instancia de la clase Estudiante
        val estudiante = new Estudiante("Juan", 20, "Ingeniería Informática")

        // Imprimimos el saludo del estudiante
        println(estudiante.saludar())

        // Imprimimos lo que está estudiando el estudiante
        println(estudiante.estudiar())

        // Creamos una instancia de la clase Profesor
        val profesor = new Profesor("María", 35, "Matemáticas")

        // Imprimimos el saludo del profesor
        println(profesor.saludar())

        // Imprimimos lo que está enseñando el profesor
        println(profesor.enseñar())
    }
}
```

Explicación del código:

1. Definimos la clase `Persona` con dos parámetros: `nombre` y `edad`. Esta clase tiene un método llamado `saludar()` que devuelve un saludo personalizado.

2. Definimos la clase `Estudiante` que hereda de la clase `Persona`. Añadimos un nuevo parámetro llamado `curso` a la clase `Estudiante`. Esta clase tiene un método llamado `estudiar()` que devuelve un mensaje indicando lo que está estudiando el estudiante.

3. Definimos la clase `Profesor` que hereda de la clase `Persona`. Añadimos un nuevo parámetro llamado `asignatura` a la clase `Profesor`. Esta clase tiene un método llamado `enseñar()` que devuelve un mensaje indicando lo que está enseñando el profesor.

4. En el objeto `Main`, creamos una instancia de la clase `Estudiante` y la llamamos `estudiante`.

5. Imprimimos el saludo del estudiante usando el método `saludar()`.

6. Imprimimos lo que está estudiando el estudiante usando el método `estudiar()`.

7. Creamos una instancia de la clase `Profesor` y la llamamos `profesor`.

8. Imprimimos el saludo del profesor usando el método `saludar()`.

9. Imprimimos lo que está enseñando el profesor usando el método `enseñar()`.

Este código muestra cómo crear clases y objetos en Scala, y cómo usar la herencia para crear nuevas clases que heredan de las clases existentes. También muestra cómo usar métodos para realizar acciones específicas en los objetos.