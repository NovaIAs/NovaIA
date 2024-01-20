```typescript
// Definición de la clase Persona
class Persona {
  nombre: string;
  edad: number;
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  saludar(): string {
    return `Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años`;
  }
}

// Definición de la clase Estudiante que hereda de Persona
class Estudiante extends Persona {
  curso: string;
  constructor(nombre: string, edad: number, curso: string) {
    super(nombre, edad);
    this.curso = curso;
  }

  estudiar(): string {
    return `Estoy estudiando ${this.curso}`;
  }
}

// Definición de la clase Profesor que hereda de Persona
class Profesor extends Persona {
  asignatura: string;
  constructor(nombre: string, edad: number, asignatura: string) {
    super(nombre, edad);
    this.asignatura = asignatura;
  }

  enseñar(): string {
    return `Estoy enseñando ${this.asignatura}`;
  }
}

// Creación de objetos de tipo Persona, Estudiante y Profesor
let persona1 = new Persona("Juan", 20);
let estudiante1 = new Estudiante("María", 18, "Ingeniería Informática");
let profesor1 = new Profesor("Pedro", 40, "Matemáticas");

// Invocación de métodos de los objetos
console.log(persona1.saludar()); // Hola, mi nombre es Juan y tengo 20 años
console.log(estudiante1.saludar()); // Hola, mi nombre es María y tengo 18 años
console.log(estudiante1.estudiar()); // Estoy estudiando Ingeniería Informática
console.log(profesor1.saludar()); // Hola, mi nombre es Pedro y tengo 40 años
console.log(profesor1.enseñar()); // Estoy enseñando Matemáticas
```

**Explicación del código:**

* Se definen las clases `Persona`, `Estudiante` y `Profesor`. La clase `Estudiante` hereda de la clase `Persona` y la clase `Profesor` hereda también de la clase `Persona`.
* Se crean objetos de tipo `Persona`, `Estudiante` y `Profesor`.
* Se invocan los métodos de los objetos creados.

Este código demuestra el uso de herencia en TypeScript. La herencia permite crear clases que heredan de otras clases y que pueden reutilizar el código de las clases base. Esto hace que el código sea más eficiente y fácil de mantener.