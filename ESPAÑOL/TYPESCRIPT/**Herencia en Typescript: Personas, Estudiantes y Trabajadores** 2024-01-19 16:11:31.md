```typescript
// Definición de la clase Persona
class Persona {
  // Propiedades públicas
  nombre: string;
  edad: number;

  // Constructor
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  // Método público
  saludar(): string {
    return `Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`;
  }
}

// Definición de la clase Estudiante
class Estudiante extends Persona {
  // Propiedades públicas
  curso: string;

  // Constructor
  constructor(nombre: string, edad: number, curso: string) {
    super(nombre, edad); // Invocación al constructor de la clase padre
    this.curso = curso;
  }

  // Método público
  estudiar(): string {
    return `Estoy estudiando ${this.curso} en la universidad.`;
  }
}

// Definición de la clase Trabajador
class Trabajador extends Persona {
  // Propiedades públicas
  profesión: string;

  // Constructor
  constructor(nombre: string, edad: number, profesión: string) {
    super(nombre, edad); // Invocación al constructor de la clase padre
    this.profesión = profesión;
  }

  // Método público
  trabajar(): string {
    return `Estoy trabajando como ${this.profesión}.`;
  }
}

// Creación de objetos
const persona1 = new Persona('Juan', 20);
const estudiante1 = new Estudiante('María', 22, 'Ingeniería Informática');
const trabajador1 = new Trabajador('Pedro', 30, 'Profesor');

// Invocación de métodos
console.log(persona1.saludar()); // Salida: "Hola, mi nombre es Juan y tengo 20 años."
console.log(estudiante1.saludar()); // Salida: "Hola, mi nombre es María y tengo 22 años."
console.log(estudiante1.estudiar()); // Salida: "Estoy estudiando Ingeniería Informática en la universidad."
console.log(trabajador1.saludar()); // Salida: "Hola, mi nombre es Pedro y tengo 30 años."
console.log(trabajador1.trabajar()); // Salida: "Estoy trabajando como Profesor."
```

Explicación:

* La clase `Persona` es una clase base que define las propiedades `nombre` y `edad`, así como el método `saludar()`.
* La clase `Estudiante` hereda de la clase `Persona` y añade la propiedad `curso` y el método `estudiar()`.
* La clase `Trabajador` hereda de la clase `Persona` y añade la propiedad `profesión` y el método `trabajar()`.
* Las clases `Estudiante` y `Trabajador` son clases derivadas de la clase `Persona`.
* Los objetos `persona1`, `estudiante1` y `trabajador1` son instancias de las clases `Persona`, `Estudiante` y `Trabajador`, respectivamente.
* Los métodos `saludar()`, `estudiar()` y `trabajar()` se invocan en los objetos `persona1`, `estudiante1` y `trabajador1`, respectivamente.