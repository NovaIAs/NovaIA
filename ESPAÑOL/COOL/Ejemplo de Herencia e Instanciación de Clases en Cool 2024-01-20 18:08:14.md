```cool

-- Definición de la clase `Persona`
class Persona {
    nombre: String;
    edad: Integer;

    constructor(nombre: String, edad: Integer) {
        this.nombre = nombre;
        this.edad = edad;
    }

    hablar() {
        println("Hola, mi nombre es", nombre, "y tengo", edad, "años.");
    }
}

-- Definición de la clase `Estudiante` que hereda de `Persona`
class Estudiante extends Persona {
    calificaciones: List<Integer>;

    constructor(nombre: String, edad: Integer, calificaciones: List<Integer>) {
        super(nombre, edad);
        this.calificaciones = calificaciones;
    }

    estudiar() {
        println("Estoy estudiando para mi examen de mañana.");
    }

    promedio() {
        var suma = 0;
        for (calificacion in calificaciones) {
            suma += calificacion;
        }
        return suma / calificaciones.size();
    }
}

-- Definición de la clase `Profesor` que hereda de `Persona`
class Profesor extends Persona {
    materias: List<String>;

    constructor(nombre: String, edad: Integer, materias: List<String>) {
        super(nombre, edad);
        this.materias = materias;
    }

    enseñar() {
        println("Estoy enseñando la clase de", materias[0], "a mis estudiantes.");
    }

    investigar() {
        println("Estoy investigando sobre un nuevo tema para mi próxima publicación.");
    }
}

-- Creación de objetos de las clases `Persona`, `Estudiante` y `Profesor`
var juan = new Persona("Juan", 20);
var maria = new Estudiante("María", 18, [9, 8, 7]);
var pedro = new Profesor("Pedro", 40, ["Matemáticas", "Física"]);

-- Llamadas a los métodos de los objetos creados
juan.hablar();
maria.hablar();
maria.estudiar();
println("El promedio de María es:", maria.promedio());
pedro.hablar();
pedro.enseñar();
pedro.investigar();

```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` tiene dos atributos, `nombre` y `edad`, y un método `hablar`.
* La clase `Estudiante` hereda de la clase `Persona` y tiene un atributo adicional llamado `calificaciones`. También tiene un método adicional llamado `estudiar` y un método llamado `promedio` que calcula el promedio de las calificaciones.
* La clase `Profesor` hereda de la clase `Persona` y tiene un atributo adicional llamado `materias`. También tiene un método adicional llamado `enseñar` y un método llamado `investigar`.
* Se crean objetos de las clases `Persona`, `Estudiante` y `Profesor` y se invocan los métodos de los objetos creados.