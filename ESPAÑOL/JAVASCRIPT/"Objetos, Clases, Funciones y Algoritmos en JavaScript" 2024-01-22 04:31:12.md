```javascript
// Crear un objeto persona con propiedades y métodos
const persona = {
  nombre: 'Juan',
  apellido: 'García',
  edad: 30,
  habilidades: ['JavaScript', 'HTML', 'CSS'],
  // Método para saludar
  saludar: function() {
    console.log(`Hola, mi nombre es ${this.nombre} ${this.apellido}.`);
  },
  // Método para añadir una nueva habilidad
  agregarHabilidad: function(nuevaHabilidad) {
    this.habilidades.push(nuevaHabilidad);
  }
};

// Imprimir el objeto persona
console.log(persona);

// Llamar al método saludar de la persona
persona.saludar();

// Añadir una nueva habilidad a la persona
persona.agregarHabilidad('React');

// Imprimir el objeto persona de nuevo
console.log(persona);

// Crear una clase Persona con propiedades y métodos
class Persona {
  constructor(nombre, apellido, edad) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.edad = edad;
  }

  // Método para saludar
  saludar() {
    console.log(`Hola, mi nombre es ${this.nombre} ${this.apellido}.`);
  }

  // Método para añadir una nueva habilidad
  agregarHabilidad(nuevaHabilidad) {
    this.habilidades.push(nuevaHabilidad);
  }
}

// Crear un objeto persona utilizando la clase Persona
const persona2 = new Persona('María', 'Fernández', 25);

// Imprimir el objeto persona2
console.log(persona2);

// Llamar al método saludar de la persona2
persona2.saludar();

// Añadir una nueva habilidad a la persona2
persona2.agregarHabilidad('Angular');

// Imprimir el objeto persona2 de nuevo
console.log(persona2);

// Crear una función para calcular el área de un triángulo
function calcularAreaTriangulo(base, altura) {
  return (base * altura) / 2;
}

// Imprimir el área de un triángulo
console.log(calcularAreaTriángulo(10, 5));

// Crear una función para calcular el factorial de un número
function calcularFactorial(numero) {
  let factorial = 1;
  for (let i = 1; i <= numero; i++) {
    factorial *= i;
  }
  return factorial;
}

// Imprimir el factorial de un número
console.log(calcularFactorial(5));

// Crear una función para comprobar si un número es primo
function esPrimo(numero) {
  if (numero <= 1) {
    return false;
  }

  for (let i = 2; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      return false;
    }
  }

  return true;
}

// Imprimir si un número es primo o no
console.log(esPrimo(13)); // true
console.log(esPrimo(20)); // false
```

Explicación del código:

- Se crea un objeto persona con propiedades y métodos utilizando la sintaxis de objetos literales.
- Se crea una clase Persona con propiedades y métodos utilizando la sintaxis de clases.
- Se crean dos objetos persona utilizando la clase Persona.
- Se llama al método saludar de las dos personas.
- Se añade una nueva habilidad a las dos personas.
- Se imprime el objeto persona y el objeto persona2.
- Se crea una función para calcular el área de un triángulo.
- Se imprime el área de un triángulo.
- Se crea una función para calcular el factorial de un número.
- Se imprime el factorial de un número.
- Se crea una función para comprobar si un número es primo.
- Se imprime si un número es primo o no.