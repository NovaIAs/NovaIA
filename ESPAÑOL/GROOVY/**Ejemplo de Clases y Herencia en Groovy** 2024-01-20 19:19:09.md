```groovy
// Declaración de una clase en Groovy.
class Persona {

    // Propiedades de la clase Persona.
    String nombre
    int edad

    // Constructor de la clase Persona.
    Persona(String nombre, int edad) {
        this.nombre = nombre
        this.edad = edad
    }

    // Método de la clase Persona.
    String saludar() {
        "Hola, mi nombre es ${nombre} y tengo ${edad} años."
    }
}

// Declaración de una clase en Groovy que extiende a la clase Persona.
class Estudiante extends Persona {

    // Propiedades de la clase Estudiante.
    String carrera

    // Constructor de la clase Estudiante.
    Estudiante(String nombre, int edad, String carrera) {
        super(nombre, edad) // Llama al constructor de la clase padre.
        // o también podemos poner this(nombre, edad)
        this.carrera = carrera
    }

    // Método de la clase Estudiante.
    String estudiar() {
        "Estoy estudiando la carrera de ${carrera}."
    }
}

// Creamos un objeto de la clase Persona.
Persona persona = new Persona("Juan", 25)

// Creamos un objeto de la clase Estudiante.
Estudiante estudiante = new Estudiante("María", 20, "Ingeniería en Computación")

// Imprimimos el saludo de la persona.
println persona.saludar()

// Imprimimos el saludo del estudiante.
println estudiante.saludar()

// Imprimimos la carrera del estudiante.
println estudiante.carrera

// Imprimimos lo que está estudiando el estudiante.
println estudiante.estudiar()
```

**Explicación del código:**

El código anterior define dos clases en Groovy: `Persona` y `Estudiante`. La clase `Persona` tiene dos propiedades, `nombre` y `edad`, y un método, `saludar()`. La clase `Estudiante` extiende a la clase `Persona` y añade una nueva propiedad, `carrera`, y un nuevo método, `estudiar()`.

Luego, creamos un objeto de la clase `Persona` y otro de la clase `Estudiante`. Utilizamos el método `saludar()` para imprimir el saludo de la persona y del estudiante. También utilizamos el método `estudiar()` para imprimir lo que está estudiando el estudiante.

Este código es complejo porque define dos clases y utiliza herencia y polimorfismo. También utiliza el concepto de encapsulamiento, ya que las propiedades y métodos de las clases son privadas.