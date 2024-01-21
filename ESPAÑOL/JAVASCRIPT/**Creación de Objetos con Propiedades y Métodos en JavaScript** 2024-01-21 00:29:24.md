```javascript
// Crear una clase de Persona con propiedades y métodos
class Persona {
  constructor(nombre, edad, ocupacion) {
    this.nombre = nombre;
    this.edad = edad;
    this.ocupacion = ocupacion;
  }

  saludar() {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años. Soy ${this.ocupacion}.`);
  }
}

// Crear una clase de Empleado que herede de Persona
class Empleado extends Persona {
  constructor(nombre, edad, ocupacion, salario) {
    super(nombre, edad, ocupacion);
    this.salario = salario;
  }

  calcularImpuestos() {
    // Calcular los impuestos del empleado según su salario
    let impuestos = this.salario * 0.1;
    return impuestos;
  }
}

// Crear una clase de Estudiante que herede de Persona
class Estudiante extends Persona {
  constructor(nombre, edad, ocupacion, calificaciones) {
    super(nombre, edad, ocupacion);
    this.calificaciones = calificaciones;
  }

  calcularPromedio() {
    // Calcular el promedio de las calificaciones del estudiante
    let promedio = 0;
    for (let i = 0; i < this.calificaciones.length; i++) {
      promedio += this.calificaciones[i];
    }
    promedio /= this.calificaciones.length;
    return promedio;
  }
}

// Crear objetos de tipo Persona, Empleado y Estudiante
let persona1 = new Persona("Juan", 25, "Estudiante");
let empleado1 = new Empleado("María", 30, "Ingeniera", 10000);
let estudiante1 = new Estudiante("Pedro", 20, "Estudiante", [8, 9, 7]);

// Llamar a los métodos de cada objeto
persona1.saludar();
empleado1.saludar();
empleado1.calcularImpuestos();
estudiante1.saludar();
estudiante1.calcularPromedio();

// El código anterior crea tres objetos diferentes: una persona, un empleado y un estudiante. Cada objeto tiene sus propias propiedades y métodos, que se pueden utilizar para realizar diferentes tareas.

// Por ejemplo, el objeto persona tiene un método llamado "saludar()" que simplemente imprime un mensaje en la consola. El objeto empleado tiene un método llamado "calcularImpuestos()" que calcula los impuestos del empleado según su salario. El objeto estudiante tiene un método llamado "calcularPromedio()" que calcula el promedio de las calificaciones del estudiante.

// Este código es un ejemplo de cómo se pueden utilizar las clases y la herencia en JavaScript para crear objetos con propiedades y métodos específicos.
```

Explicación del código:

* La clase `Persona` es una clase base que define las propiedades y métodos comunes a todas las personas, como el nombre, la edad y la ocupación.
* La clase `Empleado` es una clase derivada de la clase `Persona` que define las propiedades y métodos específicos de los empleados, como el salario y el método `calcularImpuestos()`.
* La clase `Estudiante` es otra clase derivada de la clase `Persona` que define las propiedades y métodos específicos de los estudiantes, como las calificaciones y el método `calcularPromedio()`.
* Se crean objetos de tipo `Persona`, `Empleado` y `Estudiante` utilizando el operador `new`.
* Se llaman a los métodos de cada objeto para realizar diferentes tareas, como saludar, calcular impuestos y calcular el promedio de las calificaciones.

Este código es un ejemplo de cómo se pueden usar las clases y la herencia en JavaScript para crear objetos con propiedades y métodos específicos.