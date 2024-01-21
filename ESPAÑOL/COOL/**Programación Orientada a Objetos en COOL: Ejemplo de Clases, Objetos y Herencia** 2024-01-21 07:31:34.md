```cool
clase Persona {
    nombre: String;
    edad: Integer;
    constructor(nombre: String, edad: Integer) {
        this.nombre = nombre;
        this.edad = edad;
    }
    saludar() {
        println("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
    }
}

clase Estudiante : Persona {
    matricula: String;
    constructor(nombre: String, edad: Integer, matricula: String) {
        super(nombre, edad);
        this.matricula = matricula;
    }
    estudiar() {
        println("Estoy estudiando para mi examen de mañana.");
    }
}

clase Profesor : Persona {
    asignatura: String;
    constructor(nombre: String, edad: Integer, asignatura: String) {
        super(nombre, edad);
        this.asignatura = asignatura;
    }
    enseñar() {
        println("Estoy enseñando a mis alumnos sobre " + this.asignatura + ".");
    }
}

clase Main {
    static void main() {
        var juan = new Estudiante("Juan Pérez", 18, "12345");
        var maría = new Profesor("María González", 35, "Matemáticas");

        juan.saludar();
        juan.estudiar();

        maría.saludar();
        maría.enseñar();
    }
}
```

Explicación del código:

* **Clase Persona:** Esta clase define las propiedades y métodos comunes a todas las personas, como su nombre, edad y un método para saludar.
* **Clase Estudiante:** Esta clase hereda de la clase Persona y añade propiedades y métodos específicos de los estudiantes, como su matrícula y un método para estudiar.
* **Clase Profesor:** Esta clase también hereda de la clase Persona y añade propiedades y métodos específicos de los profesores, como su asignatura y un método para enseñar.
* **Clase Main:** Esta clase contiene el método `main()` que se ejecuta al iniciar el programa. En este método se crean dos objetos, `juan` y `maría`, de las clases `Estudiante` y `Profesor` respectivamente. A continuación, se llama a los métodos `saludar()` y `estudiar()` del objeto `juan`, y a los métodos `saludar()` y `enseñar()` del objeto `maría`.

Este código es un ejemplo de programación orientada a objetos en COOL. Muestra cómo crear clases y objetos, cómo heredar de otras clases y cómo llamar a métodos de los objetos.