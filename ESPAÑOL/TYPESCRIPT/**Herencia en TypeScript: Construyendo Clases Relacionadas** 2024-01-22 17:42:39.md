```typescript
// Clase padre "Persona" con propiedades y métodos básicos
class Persona {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  hablar(): void {
    console.log("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
  }
}

// Clase hija "Estudiante" que hereda de "Persona" y añade nuevas propiedades y métodos
class Estudiante extends Persona {
  matricula: string;
  calificaciones: number[];

  constructor(nombre: string, edad: number, matricula: string, calificaciones: number[]) {
    // Llamamos al constructor de la clase padre con la palabra clave "super"
    super(nombre, edad);
    this.matricula = matricula;
    this.calificaciones = calificaciones;
  }

  estudiar(): void {
    console.log("Estoy estudiando para mi próximo examen.");
  }

  // Sobrescribimos el método "hablar()" de la clase padre
  hablar(): void {
    console.log("Hola, soy " + this.nombre + ", tengo " + this.edad + " años y mi matrícula es " + this.matricula + ".");
  }
}

// Clase hija "Profesor" que hereda de "Persona" y añade nuevas propiedades y métodos
class Profesor extends Persona {
  materia: string;
  salario: number;

  constructor(nombre: string, edad: number, materia: string, salario: number) {
    super(nombre, edad);
    this.materia = materia;
    this.salario = salario;
  }

  enseñar(): void {
    console.log("Estoy dando clase de " + this.materia + ".");
  }

  // Sobrescribimos el método "hablar()" de la clase padre
  hablar(): void {
    console.log("Hola, soy " + this.nombre + ", tengo " + this.edad + " años y enseño " + this.materia + ".");
  }
}

// Creamos una instancia de la clase "Estudiante"
let estudiante = new Estudiante("Juan García", 20, "123456", [8, 9, 10]);

// Llamamos a los métodos de la clase "Estudiante"
estudiante.hablar();
estudiante.estudiar();

// Creamos una instancia de la clase "Profesor"
let profesor = new Profesor("María Pérez", 40, "Matemáticas", 2000);

// Llamamos a los métodos de la clase "Profesor"
profesor.hablar();
profesor.enseñar();
```

**Explicación:**

El código anterior define tres clases: "Persona", "Estudiante" y "Profesor". La clase "Persona" es la clase padre, y las clases "Estudiante" y "Profesor" son clases hijas que heredan de la clase "Persona".

La clase "Persona" tiene dos propiedades: "nombre" y "edad", y un método llamado "hablar()". La clase "Estudiante" hereda las propiedades y el método de la clase "Persona", y añade dos propiedades nuevas: "matricula" y "calificaciones", y un nuevo método llamado "estudiar()". La clase "Profesor" también hereda las propiedades y el método de la clase "Persona", y añade dos propiedades nuevas: "materia" y "salario", y un nuevo método llamado "enseñar()".

En el código también creamos dos instancias de las clases "Estudiante" y "Profesor", y llamamos a sus métodos para demostrar cómo funcionan.

Este código es un ejemplo de cómo utilizar la herencia en TypeScript para crear clases relacionadas que comparten propiedades y métodos comunes.