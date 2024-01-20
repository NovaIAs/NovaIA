```groovy
// Definición de clases y métodos

// Clase Persona
class Persona {
    String nombre
    int edad

    // Constructor
    def Persona(String nombre, int edad) {
        this.nombre = nombre
        this.edad = edad
    }

    // Método para obtener el nombre de la persona
    String getNombre() {
        return nombre
    }

    // Método para obtener la edad de la persona
    int getEdad() {
        return edad
    }

    // Método para saludar
    void saludar() {
        println("Hola, mi nombre es ${nombre} y tengo ${edad} años")
    }
}

// Clase Estudiante que hereda de Persona
class Estudiante extends Persona {
    String curso

    // Constructor
    def Estudiante(String nombre, int edad, String curso) {
        super(nombre, edad)  // Llamada al constructor de la clase padre
        this.curso = curso
    }

    // Método para obtener el curso del estudiante
    String getCurso() {
        return curso
    }

    // Método para saludar al estudiante
    void saludar() {
        // Uso de super para llamar al método saludar de la clase padre
        super.saludar()
        println("Soy estudiante de ${curso}")
    }
}

// Clase Profesor que hereda de Persona
class Profesor extends Persona {
    String asignatura

    // Constructor
    def Profesor(String nombre, int edad, String asignatura) {
        super(nombre, edad)  // Llamada al constructor de la clase padre
        this.asignatura = asignatura
    }

    // Método para obtener la asignatura del profesor
    String getAsignatura() {
        return asignatura
    }

    // Método para saludar al profesor
    void saludar() {
        // Uso de super para llamar al método saludar de la clase padre
        super.saludar()
        println("Soy profesor de ${asignatura}")
    }
}

// Creación de objetos

// Creación de una persona
def persona1 = new Persona("Juan", 30)

// Creación de un estudiante
def estudiante1 = new Estudiante("María", 20, "Informática")

// Creación de un profesor
def profesor1 = new Profesor("Pedro", 40, "Matemáticas")

// Llamada a los métodos

// Impresión del nombre de la persona
println("Nombre de la persona: ${persona1.nombre}")

// Saludo de la persona
persona1.saludar()

// Impresión del nombre del estudiante
println("Nombre del estudiante: ${estudiante1.nombre}")

// Impresión del curso del estudiante
println("Curso del estudiante: ${estudiante1.curso}")

// Saludo del estudiante
estudiante1.saludar()

// Impresión del nombre del profesor
println("Nombre del profesor: ${profesor1.nombre}")

// Impresión de la asignatura del profesor
println("Asignatura del profesor: ${profesor1.asignatura}")

// Saludo del profesor
profesor1.saludar()
```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante` y `Profesor`, que representan personas, estudiantes y profesores, respectivamente. Cada clase tiene sus propios atributos y métodos.
* Se crean objetos de las clases definidas anteriormente y se les asignan valores.
* Se llaman a los métodos de los objetos creados para mostrar los valores de los atributos y para imprimir mensajes de saludo.

Este código es un ejemplo de programación orientada a objetos en Groovy. Muestra cómo definir clases, crear objetos y llamar a sus métodos.