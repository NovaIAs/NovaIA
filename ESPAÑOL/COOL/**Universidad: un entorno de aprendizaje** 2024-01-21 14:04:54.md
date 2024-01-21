```cool
-- Declaración de una clase Persona
clase Persona {
    -- Atributos
    nombre: String;
    edad: Integer;

    -- Constructor
    init(nombre: String, edad: Integer) {
        this.nombre = nombre;
        this.edad = edad;
    }

    -- Métodos
    saludar() {
        print("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
    }
}

-- Declaración de una clase Estudiante, que hereda de la clase Persona
clase Estudiante extends Persona {
    -- Atributos
    matricula: String;
    carrera: String;

    -- Constructor
    init(nombre: String, edad: Integer, matricula: String, carrera: String) {
        super.init(nombre, edad);
        this.matricula = matricula;
        this.carrera = carrera;
    }

    -- Métodos
    estudiar() {
        print("Estoy estudiando para mi examen de " + this.carrera + ".");
    }
}

-- Declaración de una clase Profesor, que hereda de la clase Persona
clase Profesor extends Persona {
    -- Atributos
    asignatura: String;
    grado: String;

    -- Constructor
    init(nombre: String, edad: Integer, asignatura: String, grado: String) {
        super.init(nombre, edad);
        this.asignatura = asignatura;
        this.grado = grado;
    }

    -- Métodos
    enseñar() {
        print("Estoy enseñando " + this.asignatura + " a mis estudiantes de " + this.grado + ".");
    }
}

-- Declaración de una clase Universidad
clase Universidad {
    -- Atributos
    nombre: String;
    facultades: List[Facultad];

    -- Constructor
    init(nombre: String, facultades: List[Facultad]) {
        this.nombre = nombre;
        this.facultades = facultades;
    }

    -- Métodos
    admitirEstudiante(estudiante: Estudiante) {
        // Se busca la facultad correspondiente a la carrera del estudiante
        facultad = this.facultades.find(facultad => facultad.carrera == estudiante.carrera);

        // Si se encuentra la facultad, se añade el estudiante a la lista de estudiantes de la facultad
        if (facultad != null) {
            facultad.estudiantes.add(estudiante);
        }
    }

    admitirProfesor(profesor: Profesor) {
        // Se busca la facultad correspondiente a la asignatura del profesor
        facultad = this.facultades.find(facultad => facultad.asignatura == profesor.asignatura);

        // Si se encuentra la facultad, se añade el profesor a la lista de profesores de la facultad
        if (facultad != null) {
            facultad.profesores.add(profesor);
        }
    }
}

-- Declaración de una clase Facultad
clase Facultad {
    -- Atributos
    nombre: String;
    carrera: String;
    asignatura: String;
    estudiantes: List[Estudiante];
    profesores: List[Profesor];

    -- Constructor
    init(nombre: String, carrera: String, asignatura: String, estudiantes: List[Estudiante], profesores: List[Profesor]) {
        this.nombre = nombre;
        this.carrera = carrera;
        this.asignatura = asignatura;
        this.estudiantes = estudiantes;
        this.profesores = profesores;
    }

    -- Métodos
    impartirClases() {
        // Cada profesor de la facultad imparte su asignatura a sus estudiantes
        for (profesor in this.profesores) {
            for (estudiante in this.estudiantes) {
                profesor.enseñar();
                estudiante.estudiar();
            }
        }
    }
}

-- Declaración de una clase ProgramaPrincipal
clase ProgramaPrincipal {
    -- Método principal
    main() {
        // Se crea una universidad
        universidad = Universidad("Universidad de Sevilla", []);

        // Se crean las facultades de la universidad
        facultadIngenieria = Facultad("Ingeniería", "Ingeniería Informática", "Programación", [], []);
        facultadCiencias = Facultad("Ciencias", "Matemáticas", "Análisis Matemático", [], []);

        // Se añaden las facultades a la universidad
        universidad.facultades.add(facultadIngenieria);
        universidad.facultades.add(facultadCiencias);

        // Se crean los estudiantes y profesores de la universidad
        estudiante1 = Estudiante("Juan", 20, "123456", "Ingeniería Informática");
        estudiante2 = Estudiante("María", 21, "654321", "Matemáticas");
        profesor1 = Profesor("Pedro", 40, "Programación", "Ingeniería Informática");
        profesor2 = Profesor("Ana", 45, "Análisis Matemático", "Matemáticas");

        // Se admiten los estudiantes y profesores en la universidad
        universidad.admitirEstudiante(estudiante1);
        universidad.admitirEstudiante(estudiante2);
        universidad.admitirProfesor(profesor1);
        universidad.admitirProfesor(profesor2);

        // Se imparten las clases en la universidad
        universidad.facultades[0].impartirClases();
        universidad.facultades[1].impartirClases();
    }
}
```

Explicación del código:

* Se declaran varias clases, incluyendo `Persona`, `Estudiante`, `Profesor`, `Universidad`, `Facultad` y `ProgramaPrincipal`.
* La clase `Persona` es una clase abstracta que define los atributos y métodos comunes a todas las personas, como el nombre y la edad.
* Las clases `Estudiante` y `Profesor` heredan de la clase `Persona` y añaden atributos y métodos específicos de cada tipo de persona.
* La clase `Universidad` representa una universidad, que tiene un nombre y una lista de facultades.
* La clase `Facultad` representa una facultad dentro de una universidad, que tiene un nombre, una carrera, una asignatura, una lista de estudiantes y una lista de profesores.
* La clase `ProgramaPrincipal` contiene el método `main`, que es el punto de entrada del programa.
* En el método `main`, se crea una universidad, se crean las facultades de la universidad, se crean los estudiantes y profesores de la universidad, se admiten los estudiantes y profesores en la universidad, y se imparten las clases en la universidad.

Este código es un ejemplo de cómo se puede utilizar el lenguaje COOL para crear un programa complejo y estructurado.