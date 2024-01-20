```cool

clase Persona {
    nombre: String;
    edad: Int;
    genero: String;

    constructor(nombre: String, edad: Int, genero: String) {
        this.nombre = nombre;
        this.edad = edad;
        this.genero = genero;
    }

    método mostrar() {
        println("Nombre: ${nombre}");
        println("Edad: ${edad}");
        println("Género: ${genero}");
    }
}

clase Estudiante: Persona {
    curso: String;
    nota_media: Float;

    constructor(nombre: String, edad: Int, genero: String, curso: String, nota_media: Float) : super(nombre, edad, genero) {
        this.curso = curso;
        this.nota_media = nota_media;
    }

    método mostrar() : super.mostrar() {
        println("Curso: ${curso}");
        println("Nota media: ${nota_media}");
    }
}

clase Profesor: Persona {
    asignatura: String;
    años_experiencia: Int;

    constructor(nombre: String, edad: Int, genero: String, asignatura: String, años_experiencia: Int) : super(nombre, edad, genero) {
        this.asignatura = asignatura;
        this.años_experiencia = años_experiencia;
    }

    método mostrar() : super.mostrar() {
        println("Asignatura: ${asignatura}");
        println("Años de experiencia: ${años_experiencia}");
    }
}

función main() {
    estudiante1 = Estudiante("Juan", 20, "Masculino", "Ingeniería Informática", 8.5);
    profesor1 = Profesor("María", 45, "Femenino", "Matemáticas", 15);

    estudiante1.mostrar();
    println();
    profesor1.mostrar();
}

```

**Explicación del código:**

* Clase `Persona`: Esta clase define las propiedades y métodos comunes a todas las personas.
* Clase `Estudiante`: Esta clase hereda de la clase `Persona` y añade las propiedades y métodos específicos de los estudiantes.
* Clase `Profesor`: Esta clase hereda de la clase `Persona` y añade las propiedades y métodos específicos de los profesores.
* Función `main()`: Esta es la función principal del programa. Crea objetos de las clases `Estudiante` y `Profesor` y llama a sus métodos para mostrar sus datos.

Este código define tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` define las propiedades y métodos comunes a todas las personas, como el nombre, la edad y el género. La clase `Estudiante` hereda de la clase `Persona` y añade las propiedades y métodos específicos de los estudiantes, como el curso y la nota media. La clase `Profesor` también hereda de la clase `Persona` y añade las propiedades y métodos específicos de los profesores, como la asignatura y los años de experiencia.

La función `main()` crea objetos de las clases `Estudiante` y `Profesor` y llama a sus métodos para mostrar sus datos.