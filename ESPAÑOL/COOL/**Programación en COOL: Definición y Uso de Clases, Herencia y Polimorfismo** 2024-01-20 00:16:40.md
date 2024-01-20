```cool
clase Persona {
  nombre : String;
  edad   : Int;
  altura : Float;

  // Constructor
  nuevo(nombre: String, edad: Int, altura: Float) {
    this.nombre = nombre;
    this.edad   = edad;
    this.altura = altura;
  }

  // Método para imprimir los datos de la persona
  imprimir() {
    println("Nombre: " + this.nombre);
    println("Edad: "   + this.edad);
    println("Altura: "  + this.altura);
  }
}

// Definición de la clase Estudiante que hereda de Persona
clase Estudiante : Persona {
  matricula : String;
  carrera   : String;

  // Constructor
  nuevo(nombre: String, edad: Int, altura: Float, matricula: String, carrera: String) {
    super.nuevo(nombre, edad, altura);
    this.matricula = matricula;
    this.carrera   = carrera;
  }

  // Método para imprimir los datos del estudiante
  imprimir() {
    // Primero se imprimen los datos de la persona
    super.imprimir();

    // Luego se imprimen los datos del estudiante
    println("Matrícula: " + this.matricula);
    println("Carrera: "   + this.carrera);
  }
}

// Definición de la clase Profesor que hereda de Persona
clase Profesor : Persona {
  materias : Array[String];
  sueldo   : Float;

  // Constructor
  nuevo(nombre: String, edad: Int, altura: Float, materias: Array[String], sueldo: Float) {
    super.nuevo(nombre, edad, altura);
    this.materias = materias;
    this.sueldo   = sueldo;
  }

  // Método para imprimir los datos del profesor
  imprimir() {
    // Primero se imprimen los datos de la persona
    super.imprimir();

    // Luego se imprimen los datos del profesor
    println("Materias:");
    for (materia in this.materias) {
      println("  - " + materia);
    }

    println("Sueldo: " + this.sueldo);
  }
}

// Creación de un objeto Persona
persona1 = new Persona("Juan Pérez", 20, 1.75);

// Creación de un objeto Estudiante
estudiante1 = new Estudiante("María López", 18, 1.65, "12345", "Ingeniería en Informática");

// Creación de un objeto Profesor
profesor1 = new Profesor("Carlos García", 40, 1.80, ["Programación", "Bases de Datos", "Redes"], 2000.0);

// Impresión de los datos de la persona
persona1.imprimir();

// Impresión de los datos del estudiante
estudiante1.imprimir();

// Impresión de los datos del profesor
profesor1.imprimir();
```

El código anterior define tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` define los atributos comunes a todas las personas, como el nombre, la edad y la altura. La clase `Estudiante` hereda de la clase `Persona` y añade atributos específicos de los estudiantes, como la matrícula y la carrera. La clase `Profesor` también hereda de la clase `Persona` y añade atributos específicos de los profesores, como las materias que imparten y el sueldo.

El código también crea tres objetos: una persona, un estudiante y un profesor. Los tres objetos comparten los atributos comunes de la clase `Persona`, pero también tienen atributos específicos de sus respectivas clases.

Finalmente, el código imprime los datos de los tres objetos. Para ello, utiliza el método `imprimir()` definido en cada clase. El método `imprimir()` de la clase `Persona` imprime los datos comunes a todas las personas, mientras que los métodos `imprimir()` de las clases `Estudiante` y `Profesor` imprimen los datos específicos de los estudiantes y profesores, respectivamente.