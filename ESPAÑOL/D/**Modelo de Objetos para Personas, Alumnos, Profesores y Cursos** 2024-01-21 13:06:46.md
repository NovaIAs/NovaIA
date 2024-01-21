```d
import std.stdio, std.string;

class Persona {
    string nombre;
    int edad;

    Persona() { }
    Persona(string nombre, int edad) {
        this.nombre = nombre;
        this.edad = edad;
    }

    string toString() {
        return "[Persona] Nombre: " ~ nombre ~ ", Edad: " ~ edad.toString();
    }
}

class Alumno : Persona {
    string matricula;
    string carrera;

    Alumno() { }
    Alumno(string nombre, int edad, string matricula, string carrera) : super(nombre, edad) {
        this.matricula = matricula;
        this.carrera = carrera;
    }

    string toString() {
        return super.toString() ~ ", Matrícula: " ~ matricula ~ ", Carrera: " ~ carrera;
    }
}

class Profesor : Persona {
    string grado;
    string departamento;

    Profesor() { }
    Profesor(string nombre, int edad, string grado, string departamento) : super(nombre, edad) {
        this.grado = grado;
        this.departamento = departamento;
    }

    string toString() {
        return super.toString() ~ ", Grado: " ~ grado ~ ", Departamento: " ~ departamento;
    }
}

class Curso {
    string nombre;
    int creditos;
    Alumno[] alumnos;
    Profesor[] profesores;

    Curso() { }
    Curso(string nombre, int creditos, Alumno[] alumnos, Profesor[] profesores) {
        this.nombre = nombre;
        this.creditos = creditos;
        this.alumnos = alumnos;
        this.profesores = profesores;
    }

    string toString() {
        string alumnosString = "";
        foreach (alumno; alumnos) {
            alumnosString += alumno.toString() ~ "\n";
        }

        string profesoresString = "";
        foreach (profesor; profesores) {
            profesoresString += profesor.toString() ~ "\n";
        }

        return "[Curso] Nombre: " ~ nombre ~ ", Créditos: " ~ creditos.toString() ~ "\n" ~
            "Alumnos:\n" ~ alumnosString ~
            "Profesores:\n" ~ profesoresString;
    }
}

void main() {
    Alumno[] alumnos = new Alumno[2];
    alumnos[0] = new Alumno("Juan Pérez", 20, "123456", "Ingeniería en Informática");
    alumnos[1] = new Alumno("María Gómez", 21, "654321", "Licenciatura en Derecho");

    Profesor[] profesores = new Profesor[2];
    profesores[0] = new Profesor("Dr. Carlos García", 45, "Profesor Titular", "Departamento de Informática");
    profesores[1] = new Profesor("Lic. Ana López", 35, "Profesora Adjunta", "Departamento de Derecho");

    Curso curso = new Curso("Fundamentos de Programación", 3, alumnos, profesores);

    writefln(curso.toString());
}
```

Este código crea un modelo de objeto para representar a personas, alumnos y profesores, y un modelo de objeto para representar a un curso. El modelo de objeto de persona tiene dos propiedades: nombre y edad. El modelo de objeto de alumno hereda del modelo de objeto de persona y agrega dos propiedades más: matrícula y carrera. El modelo de objeto de profesor también hereda del modelo de objeto de persona y agrega dos propiedades más: grado y departamento. El modelo de objeto de curso tiene cuatro propiedades: nombre, creditos, alumnos y profesores.

La función `main()` crea dos objetos de alumno, dos objetos de profesor y un objeto de curso. Luego, imprime el objeto de curso en la consola.

La salida de la función `main()` será similar a la siguiente:

```
[Curso] Nombre: Fundamentos de Programación, Créditos: 3
Alumnos:
[Persona] Nombre: Juan Pérez, Edad: 20, Matrícula: 123456, Carrera: Ingeniería en Informática
[Persona] Nombre: María Gómez, Edad: 21, Matrícula: 654321, Carrera: Licenciatura en Derecho
Profesores:
[Persona] Nombre: Dr. Carlos García, Edad: 45, Grado: Profesor Titular, Departamento: Departamento de Informática
[Persona] Nombre: Lic. Ana López, Edad: 35, Grado: Profesora Adjunta, Departamento: Departamento de Derecho
```