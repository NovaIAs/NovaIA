```cool
-- Comentario de una línea

/* Comentario
  de
  múltiples
  líneas */

clase [Persona] {
    nombre: String;
    edad: Int;

    constructor [new (nombre: String, edad: Int)] {
        this.nombre := nombre;
        this.edad := edad;
    }

    metodo [saludo () {
        println("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
    }
}

clase [Estudiante] hereda [Persona] {
    carrera: String;

    constructor [new (nombre: String, edad: Int, carrera: String)] {
        super(nombre, edad);
        this.carrera := carrera;
    }

    metodo [saludo () {
        super.saludo();
        println("Estudio la carrera de " + this.carrera + ".");
    }
}

clase [Profesor] hereda [Persona] {
    asignatura: String;

    constructor [new (nombre: String, edad: Int, asignatura: String)] {
        super(nombre, edad);
        this.asignatura := asignatura;
    }

    metodo [saludo () {
        super.saludo();
        println("Soy profesor de " + this.asignatura + ".");
    }
}

clase [Principal] {
    inicio () {
        persona1 := Persona.new("Juan", 20);
        persona2 := Estudiante.new("María", 22, "Ingeniería Informática");
        persona3 := Profesor.new("Pedro", 30, "Matemáticas");

        persona1.saludo();
        persona2.saludo();
        persona3.saludo();
    }
}
```

Explicación:

* La clase `Persona` define los atributos y métodos comunes a todas las personas.
* La clase `Estudiante` hereda de la clase `Persona` y define los atributos y métodos específicos de los estudiantes.
* La clase `Profesor` hereda de la clase `Persona` y define los atributos y métodos específicos de los profesores.
* La clase `Principal` es la clase principal del programa. El método `inicio` es el punto de entrada del programa.
* En el método `inicio` se crean tres objetos: una persona, un estudiante y un profesor.
* A continuación, se llama al método `saludo` de cada objeto.
* El método `saludo` de la clase `Persona` imprime un mensaje con el nombre y la edad de la persona.
* El método `saludo` de la clase `Estudiante` imprime un mensaje con el nombre, la edad y la carrera del estudiante.
* El método `saludo` de la clase `Profesor` imprime un mensaje con el nombre, la edad y la asignatura del profesor.