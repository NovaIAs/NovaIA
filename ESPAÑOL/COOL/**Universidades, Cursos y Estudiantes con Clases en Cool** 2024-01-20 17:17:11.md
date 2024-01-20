```cool
clase Estudiante {
    nombre: String;
    edad: Int;
    calificaciones: List[Int];

    constructor(nombre: String, edad: Int, calificaciones: List[Int]) {
        this.nombre = nombre;
        this.edad = edad;
        this.calificaciones = calificaciones;
    }

    getCalificacionMedia(): Int {
        var total = 0;
        for (nota in calificaciones) {
            total += nota;
        }
        return total / calificaciones.length;
    }
}

clase Curso {
    nombre: String;
    estudiantes: List[Estudiante];

    constructor(nombre: String, estudiantes: List[Estudiante]) {
        this.nombre = nombre;
        this.estudiantes = estudiantes;
    }

    getEstudianteConMayorCalificacionMedia(): Estudiante {
        var estudianteConMayorCalificacionMedia: Estudiante = estudiantes[0];
        var mayorCalificacionMedia = estudianteConMayorCalificacionMedia.getCalificacionMedia();

        for (estudiante in estudiantes) {
            var calificacionMediaActual = estudiante.getCalificacionMedia();
            if (calificacionMediaActual > mayorCalificacionMedia) {
                estudianteConMayorCalificacionMedia = estudiante;
                mayorCalificacionMedia = calificacionMediaActual;
            }
        }

        return estudianteConMayorCalificacionMedia;
    }
}

clase Universidad {
    nombre: String;
    cursos: List[Curso];

    constructor(nombre: String, cursos: List[Curso]) {
        this.nombre = nombre;
        this.cursos = cursos;
    }

    getCursoConMayorPromedioDeCalificaciones(): Curso {
        var cursoConMayorPromedioDeCalificaciones: Curso = cursos[0];
        var mayorPromedioDeCalificaciones = cursoConMayorPromedioDeCalificaciones.getPromedioDeCalificaciones();

        for (curso in cursos) {
            var promedioDeCalificacionesActual = curso.getPromedioDeCalificaciones();
            if (promedioDeCalificacionesActual > mayorPromedioDeCalificaciones) {
                cursoConMayorPromedioDeCalificaciones = curso;
                mayorPromedioDeCalificaciones = promedioDeCalificacionesActual;
            }
        }

        return cursoConMayorPromedioDeCalificaciones;
    }
}

// Crear estudiantes
var estudiante1 = new Estudiante("Juan", 20, [8, 9, 10]);
var estudiante2 = new Estudiante("María", 21, [9, 8, 7]);
var estudiante3 = new Estudiante("Pedro", 22, [7, 8, 9]);

// Crear cursos
var curso1 = new Curso("Matemáticas", [estudiante1, estudiante2]);
var curso2 = new Curso("Física", [estudiante2, estudiante3]);

// Crear universidad
var universidad = new Universidad("Universidad de la Ciudad", [curso1, curso2]);

// Mostrar el curso con mayor promedio de calificaciones
var cursoConMayorPromedioDeCalificaciones = universidad.getCursoConMayorPromedioDeCalificaciones();
println("El curso con mayor promedio de calificaciones es " + cursoConMayorPromedioDeCalificaciones.nombre);

// Mostrar el estudiante con mayor calificación media del curso con mayor promedio de calificaciones
var estudianteConMayorCalificacionMedia = cursoConMayorPromedioDeCalificaciones.getEstudianteConMayorCalificacionMedia();
println("El estudiante con mayor calificación media es " + estudianteConMayorCalificacionMedia.nombre);
```

Este código crea una clase `Estudiante` con los atributos `nombre`, `edad` y `calificaciones`. También crea una clase `Curso` con los atributos `nombre` y `estudiantes`. Por último, crea una clase `Universidad` con los atributos `nombre` y `cursos`.

A continuación, el código crea tres objetos de la clase `Estudiante`, dos objetos de la clase `Curso` y un objeto de la clase `Universidad`.

El código luego usa los métodos `getCursoConMayorPromedioDeCalificaciones()` y `getEstudianteConMayorCalificacionMedia()` para encontrar el curso con mayor promedio de calificaciones y el estudiante con mayor calificación media del curso con mayor promedio de calificaciones.

Por último, el código usa la función `println()` para mostrar el nombre del curso con mayor promedio de calificaciones y el nombre del estudiante con mayor calificación media del curso con mayor promedio de calificaciones.