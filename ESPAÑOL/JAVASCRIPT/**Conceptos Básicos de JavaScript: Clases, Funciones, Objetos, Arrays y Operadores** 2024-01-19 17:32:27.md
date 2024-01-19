```javascript
// Definir una clase Persona con propiedades y métodos básicos
class Persona {
  constructor(nombre, edad, ciudad) {
    this.nombre = nombre;
    this.edad = edad;
    this.ciudad = ciudad;
  }

  saludar() {
    console.log(`Hola, me llamo ${this.nombre} y tengo ${this.edad} años. Soy de ${this.ciudad}.`);
  }
}

// Crear una instancia de la clase Persona
const persona1 = new Persona("Juan", 25, "Madrid");

// Invocar el método saludar() de la instancia persona1
persona1.saludar();

// Definir una función para calcular el área de un triángulo
const calcularAreaTriangulo = (base, altura) => {
  return (base * altura) / 2;
};

// Invocar la función calcularAreaTriangulo() y almacenar el resultado en una variable
const areaTriangulo = calcularAreaTriangulo(5, 10);

// Mostrar el resultado en la consola
console.log(`El área del triángulo es ${areaTriangulo} unidades cuadradas.`);

// Definir un objeto literal con propiedades y métodos
const mascota = {
  nombre: "Firulais",
  especie: "Perro",
  raza: "Pastor alemán",
  edad: 3,
  ladrar() {
    console.log("Guau, guau!");
  },
};

// Acceder a las propiedades y métodos del objeto mascota
console.log(`El nombre de la mascota es ${mascota.nombre}.`);
console.log(`La especie de la mascota es ${mascota.especie}.`);
mascota.ladrar();

// Crear un array de números y aplicar la función map() para duplicar cada elemento
const numeros = [1, 2, 3, 4, 5];
const numerosDuplicados = numeros.map((numero) => numero * 2);

// Mostrar el array original y el array duplicado en la consola
console.log(`Array original: ${numeros}`);
console.log(`Array duplicado: ${numerosDuplicados}`);

// Usar la sintaxis de operador condicional ternario para determinar si un número es par o impar
const numero = 7;
const esPar = (numero % 2 === 0) ? "par" : "impar";

// Mostrar el resultado en la consola
console.log(`El número ${numero} es ${esPar}.`);
```

Este código es extenso y variado, e incluye una variedad de conceptos de JavaScript, como clases, funciones, objetos literales, arrays, operadores condicionales y sintaxis ternaria. También incluye comentarios en español para explicar el propósito de cada parte del código.

Es probable que este código no se repita exactamente de la misma manera, ya que es bastante específico y complejo. Sin embargo, los conceptos que se utilizan en este código son fundamentales para JavaScript y se pueden aplicar en una variedad de situaciones.