```cool
clase Persona {
  nombre: String;
  edad: Int;

  constructor(nombre: String, edad: Int) {
    this.nombre = nombre;
    this.edad = edad;
  }

  metodoSaludar() {
    println("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
  }
}

clase Estudiante : Persona {
  calificaciones: List<Int>;

  constructor(nombre: String, edad: Int, calificaciones: List<Int>) {
    super(nombre, edad);
    this.calificaciones = calificaciones;
  }

  metodoCalcularPromedio() {
    var suma = 0;
    for (calificacion in this.calificaciones) {
      suma += calificacion;
    }
    return suma / this.calificaciones.length;
  }
}

clase Profesor : Persona {
  asignaturas: List<String>;

  constructor(nombre: String, edad: Int, asignaturas: List<String>) {
    super(nombre, edad);
    this.asignaturas = asignaturas;
  }

  metodoMostrarAsignaturas() {
    for (asignatura in this.asignaturas) {
      println("Asignatura: " + asignatura);
    }
  }
}

clase Escuela {
  nombre: String;
  estudiantes: List<Estudiante>;
  profesores: List<Profesor>;

  constructor(nombre: String, estudiantes: List<Estudiante>, profesores: List<Profesor>) {
    this.nombre = nombre;
    this.estudiantes = estudiantes;
    this.profesores = profesores;
  }

  metodoMostrarEstudiantes() {
    for (estudiante in this.estudiantes) {
      println("Estudiante: " + estudiante.nombre);
    }
  }

  metodoMostrarProfesores() {
    for (profesor in this.profesores) {
      println("Profesor: " + profesor.nombre);
    }
  }
}

var escuela = new Escuela("Escuela Superior Politécnica", [
  new Estudiante("Juan Pérez", 18, [8, 9, 10]),
  new Estudiante("María González", 19, [7, 8, 9]),
  new Estudiante("Pedro Sánchez", 20, [6, 7, 8])
], [
  new Profesor("Ana López", 40, ["Matemáticas", "Física"]),
  new Profesor("Luis García", 45, ["Lengua", "Historia"]),
  new Profesor("Marta Sánchez", 50, ["Biología", "Química"])
]);

escuela.metodoMostrarEstudiantes();
escuela.metodoMostrarProfesores();

var estudiante1 = escuela.estudiantes[0];
println("Nombre del estudiante: " + estudiante1.nombre);
println("Edad del estudiante: " + estudiante1.edad);
println("Calificaciones del estudiante:");
for (calificacion in estudiante1.calificaciones) {
  println(calificacion);
}

var promedioEstudiante1 = estudiante1.metodoCalcularPromedio();
println("Promedio del estudiante: " + promedioEstudiante1);

var profesor1 = escuela.profesores[0];
println("Nombre del profesor: " + profesor1.nombre);
println("Edad del profesor: " + profesor1.edad);
println("Asignaturas del profesor:");
profesor1.metodoMostrarAsignaturas();
```

Este código define tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` tiene dos atributos, `nombre` y `edad`, y un método `metodoSaludar()`. La clase `Estudiante` hereda de la clase `Persona` y añade un atributo `calificaciones` y un método `metodoCalcularPromedio()`. La clase `Profesor` también hereda de la clase `Persona` y añade un atributo `asignaturas` y un método `metodoMostrarAsignaturas()`.

Luego, el código define una clase `Escuela` que tiene tres atributos: `nombre`, `estudiantes` y `profesores`. El atributo `nombre` es una cadena de caracteres que representa el nombre de la escuela. El atributo `estudiantes` es una lista de objetos de la clase `Estudiante`. El atributo `profesores` es una lista de objetos de la clase `Profesor`.

A continuación, el código crea una instancia de la clase `Escuela` llamada `escuela`. La instancia `escuela` tiene tres argumentos: el nombre de la escuela, una lista de objetos de la clase `Estudiante` y una lista de objetos de la clase `Profesor`.

Luego, el código llama a los métodos `metodoMostrarEstudiantes()` y `metodoMostrarProfesores()` de la instancia `escuela`. Estos métodos imprimen en la consola los nombres de los estudiantes y los profesores de la escuela, respectivamente.

Por último, el código crea una instancia de la clase `Estudiante` llamada `estudiante1`. Luego, llama a los métodos `metodoSaludar()`, `metodoCalcularPromedio()` y `metodoMostrarCalificaciones()` de la instancia `estudiante1`. Estos métodos imprimen en la consola el nombre, la edad, el promedio y las calificaciones del estudiante, respectivamente.