```typescript
// Definición de la clase Persona
class Persona {
  // Atributos privados
  private _nombre: string;
  private _edad: number;

  // Constructor
  constructor(nombre: string, edad: number) {
    this._nombre = nombre;
    this._edad = edad;
  }

  // Métodos públicos
  public saludar(): void {
    console.log(`Hola, mi nombre es ${this._nombre} y tengo ${this._edad} años.`);
  }
}

// Definición de la clase Estudiante
class Estudiante extends Persona {
  // Atributos privados
  private _matricula: string;
  private _calificaciones: number[];

  // Constructor
  constructor(nombre: string, edad: number, matricula: string, calificaciones: number[]) {
    super(nombre, edad);
    this._matricula = matricula;
    this._calificaciones = calificaciones;
  }
  
  // Método público
  public obtenerPromedio(): number {
    let suma = 0;
    for (let i = 0; i < this._calificaciones.length; i++) {
      suma += this._calificaciones[i];
    }
    return suma / this._calificaciones.length;
  }
}

// Definición de la clase Profesor
class Profesor extends Persona {
  // Atributos privados
  private _asignatura: string;
  private _añosExperiencia: number;

  // Constructor
  constructor(nombre: string, edad: number, asignatura: string, añosExperiencia: number) {
    super(nombre, edad);
    this._asignatura = asignatura;
    this._añosExperiencia = añosExperiencia;
  }
  
  // Método público
  public darClase(): void {
    console.log(`Estoy dando clase de ${this._asignatura}.`);
  }
}

// Definición de la clase Escuela
class Escuela {
  // Atributos privados
  private _nombre: string;
  private _alumnos: Estudiante[];
  private _profesores: Profesor[];

  // Constructor
  constructor(nombre: string, alumnos: Estudiante[], profesores: Profesor[]) {
    this._nombre = nombre;
    this._alumnos = alumnos;
    this._profesores = profesores;
  }

  // Métodos públicos
  public obtenerListaAlumnos(): Estudiante[] {
    return this._alumnos;
  }

  public obtenerListaProfesores(): Profesor[] {
    return this._profesores;
  }

  public agregarAlumno(alumno: Estudiante): void {
    this._alumnos.push(alumno);
  }

  public agregarProfesor(profesor: Profesor): void {
    this._profesores.push(profesor);
  }
}

// Crear una escuela
const escuela = new Escuela("Escuela Politécnica Nacional", [], []);

// Crear estudiantes
const estudiante1 = new Estudiante("Juan", 18, "123456", [8, 9, 10]);
const estudiante2 = new Estudiante("María", 19, "123457", [7, 8, 9]);

// Crear profesores
const profesor1 = new Profesor("Pedro", 45, "Matemáticas", 10);
const profesor2 = new Profesor("Ana", 35, "Física", 5);

// Agregar estudiantes y profesores a la escuela
escuela.agregarAlumno(estudiante1);
escuela.agregarAlumno(estudiante2);
escuela.agregarProfesor(profesor1);
escuela.agregarProfesor(profesor2);

// Imprimir la lista de alumnos y profesores de la escuela
console.log("Lista de alumnos:");
for (let i = 0; i < escuela.obtenerListaAlumnos().length; i++) {
  console.log(escuela.obtenerListaAlumnos()[i].saludar());
}

console.log("Lista de profesores:");
for (let i = 0; i < escuela.obtenerListaProfesores().length; i++) {
  console.log(escuela.obtenerListaProfesores()[i].saludar());
}
```

**Explicación del código:**

* Se definen las clases `Persona`, `Estudiante`, `Profesor` y `Escuela`.
* Las clases `Estudiante` y `Profesor` heredan de la clase `Persona`.
* Las clases tienen atributos privados, que solo pueden ser accedidos desde dentro de la clase, y métodos públicos, que pueden ser accedidos desde fuera de la clase.
* Se crean objetos de las clases `Estudiante`, `Profesor` y `Escuela`.
* Se agregan los objetos de las clases `Estudiante` y `Profesor` a la lista de alumnos y profesores de la escuela, respectivamente.
* Se imprimen la lista de alumnos y profesores de la escuela.

Este código es un ejemplo de cómo se puede utilizar la herencia y la encapsulación en TypeScript para crear un programa complejo y estructurado.