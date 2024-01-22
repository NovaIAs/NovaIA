```typescript
// Crear una clase Persona con atributos y métodos
class Persona {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  hablar(): void {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

// Crear una clase Estudiante que hereda de Persona
class Estudiante extends Persona {
  curso: string;
  promedio: number;

  constructor(nombre: string, edad: number, curso: string, promedio: number) {
    super(nombre, edad);
    this.curso = curso;
    this.promedio = promedio;
  }

  estudiar(): void {
    console.log(`${this.nombre} está estudiando para el examen final de ${this.curso}.`);
  }
}

// Crear una clase Profesor que hereda de Persona
class Profesor extends Persona {
  asignatura: string;
  salario: number;

  constructor(nombre: string, edad: number, asignatura: string, salario: number) {
    super(nombre, edad);
    this.asignatura = asignatura;
    this.salario = salario;
  }

  enseñar(): void {
    console.log(`${this.nombre} está enseñando la asignatura de ${this.asignatura} a sus alumnos.`);
  }
}

// Crear un objeto de tipo Persona
const persona1 = new Persona('Juan', 30);
persona1.hablar(); // Salida: Hola, mi nombre es Juan y tengo 30 años.

// Crear un objeto de tipo Estudiante
const estudiante1 = new Estudiante('María', 20, 'Matemáticas', 9.5);
estudiante1.hablar(); // Salida: Hola, mi nombre es María y tengo 20 años.
estudiante1.estudiar(); // Salida: María está estudiando para el examen final de Matemáticas.

// Crear un objeto de tipo Profesor
const profesor1 = new Profesor('Pedro', 45, 'Física', 2500);
profesor1.hablar(); // Salida: Hola, mi nombre es Pedro y tengo 45 años.
profesor1.enseñar(); // Salida: Pedro está enseñando la asignatura de Física a sus alumnos.

// Crear un array de objetos de tipo Persona
const personas: Persona[] = [persona1, estudiante1, profesor1];

// Recorrer el array de objetos e imprimir sus nombres y edades
for (const persona of personas) {
  console.log(`Nombre: ${persona.nombre}, Edad: ${persona.edad}`);
}

// Salida:
// Nombre: Juan, Edad: 30
// Nombre: María, Edad: 20
// Nombre: Pedro, Edad: 45
```

Explicación del código:

* Se crean tres clases: `Persona`, `Estudiante` y `Profesor`, que representan a tres tipos diferentes de personas. La clase `Persona` es la clase base, y las clases `Estudiante` y `Profesor` heredan de ella.
* Cada clase tiene sus propios atributos y métodos. Por ejemplo, la clase `Persona` tiene los atributos `nombre` y `edad`, y el método `hablar()`. La clase `Estudiante` hereda estos atributos y métodos de la clase `Persona`, y además tiene sus propios atributos (`curso` y `promedio`) y métodos (`estudiar()`). La clase `Profesor` también hereda los atributos y métodos de la clase `Persona`, y además tiene sus propios atributos (`asignatura` y `salario`) y métodos (`enseñar()`).
* Se crean tres objetos: `persona1`, `estudiante1` y `profesor1`, que son instancias de las clases `Persona`, `Estudiante` y `Profesor`, respectivamente.
* Se llaman a los métodos `hablar()`, `estudiar()` y `enseñar()` de los objetos `persona1`, `estudiante1` y `profesor1`, respectivamente.
* Se crea un array `personas` que contiene los objetos `persona1`, `estudiante1` y `profesor1`.
* Se recorre el array `personas` e se imprime el nombre y la edad de cada persona.

Este código es un ejemplo de cómo se pueden utilizar las clases y la herencia en TypeScript para crear objetos que representan a diferentes tipos de personas y que tienen diferentes atributos y métodos.