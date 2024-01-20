```cool
clase Persona {
    nombre: String;
    edad: Int;

    constructor(nombre: String, edad: Int) {
        this.nombre = nombre;
        this.edad = edad;
    }

    método saludar() {
        println("Hola, mi nombre es " + nombre + " y tengo " + edad + " años.");
    }
}

clase Estudiante: Persona {
    carrera: String;

    constructor(nombre: String, edad: Int, carrera: String) {
        super(nombre, edad);
        this.carrera = carrera;
    }

    método estudiar() {
        println("Estoy estudiando " + carrera + ".");
    }
}

clase Profesor: Persona {
    asignatura: String;

    constructor(nombre: String, edad: Int, asignatura: String) {
        super(nombre, edad);
        this.asignatura = asignatura;
    }

    método enseñar() {
        println("Estoy enseñando " + asignatura + ".");
    }
}

clase Main {
    método main() {
        persona1 = new Persona("Juan", 20);
        persona1.saludar();

        estudiante1 = new Estudiante("María", 22, "Informática");
        estudiante1.saludar();
        estudiante1.estudiar();

        profesor1 = new Profesor("Pedro", 30, "Matemáticas");
        profesor1.saludar();
        profesor1.enseñar();
    }
}
```

Explicación:

* La clase `Persona` es una clase base que define las propiedades y métodos comunes a todas las personas, como el nombre y la edad.
* La clase `Estudiante` es una subclase de `Persona` que define las propiedades y métodos específicos de los estudiantes, como la carrera.
* La clase `Profesor` es una subclase de `Persona` que define las propiedades y métodos específicos de los profesores, como la asignatura.
* La clase `Main` es la clase principal del programa, que contiene el método `main()` que se ejecuta cuando se ejecuta el programa.
* En el método `main()`, se crean tres objetos: una persona, un estudiante y un profesor.
* Se llama al método `saludar()` de cada objeto para mostrar un mensaje de saludo.
* Se llama al método `estudiar()` del objeto estudiante para mostrar un mensaje de que está estudiando.
* Se llama al método `enseñar()` del objeto profesor para mostrar un mensaje de que está enseñando.