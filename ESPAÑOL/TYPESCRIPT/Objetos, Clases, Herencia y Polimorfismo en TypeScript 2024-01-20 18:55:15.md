```typescript
// Crear un objeto con propiedades y métodos

const persona = {
  nombre: "Juan",
  edad: 25,
  hablar: function() {
    console.log("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
  }
};

// Acceder a las propiedades y a los métodos de un objeto

console.log(persona.nombre); // Juan
console.log(persona.edad); // 25
persona.hablar(); // Hola, mi nombre es Juan y tengo 25 años.

// Crear una clase

class Persona {
  constructor(nombre, edad) {
    this.nombre = nombre;
    this.edad = edad;
  }

  hablar() {
    console.log("Hola, mi nombre es " + this.nombre + " y tengo " + this.edad + " años.");
  }
}

// Crear una instancia de una clase

const persona1 = new Persona("Juan", 25);
const persona2 = new Persona("María", 30);

// Acceder a las propiedades y a los métodos de una instancia de una clase

console.log(persona1.nombre); // Juan
console.log(persona1.edad); // 25
persona1.hablar(); // Hola, mi nombre es Juan y tengo 25 años.

console.log(persona2.nombre); // María
console.log(persona2.edad); // 30
persona2.hablar(); // Hola, mi nombre es María y tengo 30 años.

// Herencia

class Estudiante extends Persona {
  constructor(nombre, edad, curso) {
    super(nombre, edad);
    this.curso = curso;
  }

  estudiar() {
    console.log("Estoy estudiando el curso " + this.curso);
  }
}

// Crear una instancia de una clase hija

const estudiante1 = new Estudiante("Juan", 25, "Ingeniería Informática");

// Acceder a las propiedades y a los métodos de una instancia de una clase hija

console.log(estudiante1.nombre); // Juan
console.log(estudiante1.edad); // 25
console.log(estudiante1.curso); // Ingeniería Informática
estudiante1.hablar(); // Hola, mi nombre es Juan y tengo 25 años.
estudiante1.estudiar(); // Estoy estudiando el curso Ingeniería Informática

// Polimorfismo

class Animal {
  constructor(nombre) {
    this.nombre = nombre;
  }

  hablar() {
    console.log("Hola, soy un animal y mi nombre es " + this.nombre);
  }
}

class Perro extends Animal {
  constructor(nombre) {
    super(nombre);
  }

  ladrar() {
    console.log("Woof!");
  }
}

class Gato extends Animal {
  constructor(nombre) {
    super(nombre);
  }

  maullar() {
    console.log("Meow!");
  }
}

// Crear instancias de las clases Animal, Perro y Gato

const animal1 = new Animal("Animal");
const perro1 = new Perro("Perro");
const gato1 = new Gato("Gato");

// Llamar al método hablar() de cada instancia

animal1.hablar(); // Hola, soy un animal y mi nombre es Animal
perro1.hablar(); // Hola, soy un animal y mi nombre es Perro
gato1.hablar(); // Hola, soy un animal y mi nombre es Gato

// Llamar al método ladrar() o maullar() de las instancias de las clases Perro y Gato

perro1.ladrar(); // Woof!
gato1.maullar(); // Meow!
```

Este código es un ejemplo de un código complejo en TypeScript que incluye la creación de objetos, clases, herencia y polimorfismo. El código está en español y está explicado de forma detallada.