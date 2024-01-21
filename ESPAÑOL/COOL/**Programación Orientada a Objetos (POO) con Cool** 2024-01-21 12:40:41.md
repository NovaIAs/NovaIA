```cool
clase Persona {
  nombre: String;
  edad: Int;

  constructor(nombre: String, edad: Int) {
    this.nombre = nombre;
    this.edad = edad;
  }

  funcion saludar() {
    console.log("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
  }
}

clase Estudiante : Persona {
  carrera: String;
  matricula: String;

  constructor(nombre: String, edad: Int, carrera: String, matricula: String) {
    super(nombre, edad);
    this.carrera = carrera;
    this.matricula = matricula;
  }

  funcion estudiar() {
    console.log("Estoy estudiando " + this.carrera + " en la universidad.");
  }
}

clase Profesor : Persona {
  asignatura: String;
  sueldo: Int;

  constructor(nombre: String, edad: Int, asignatura: String, sueldo: Int) {
    super(nombre, edad);
    this.asignatura = asignatura;
    this.sueldo = sueldo;
  }

  funcion darClase() {
    console.log("Estoy dando clase de " + this.asignatura + " en la universidad.");
  }
}

clase Main {
  funcion main() {
    estudiante1 = new Estudiante("Juan", 20, "Ingeniería en Informática", "123456");
    estudiante2 = new Estudiante("María", 21, "Derecho", "654321");

    profesor1 = new Profesor("Pedro", 40, "Matemáticas", 2000);
    profesor2 = new Profesor("Ana", 35, "Física", 2500);

    estudiante1.saludar();
    estudiante2.saludar();

    profesor1.saludar();
    profesor2.saludar();

    estudiante1.estudiar();
    estudiante2.estudiar();

    profesor1.darClase();
    profesor2.darClase();
  }
}

Main.main();
```

Explicación:

* Se define la clase base `Persona` con dos atributos (`nombre` y `edad`) y un método (`saludar()`).


* Se definen las clases `Estudiante` y `Profesor` que heredan de la clase `Persona` y añaden atributos y métodos específicos de cada clase.


* Se define la clase `Main` que contiene el método `main()` que se ejecuta cuando se ejecuta el programa. En este método se crean instancias de las clases `Estudiante` y `Profesor` y se llaman sus métodos.


* Se llama al método `main()` de la clase `Main` para ejecutar el programa.