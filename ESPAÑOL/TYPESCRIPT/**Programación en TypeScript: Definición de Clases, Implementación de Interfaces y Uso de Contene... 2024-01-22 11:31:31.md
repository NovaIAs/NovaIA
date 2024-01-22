```typescript
// Esta es una clase llamada "Persona" que define las características y comportamientos comunes de una persona.
class Persona {
  // Propiedades:
  nombre: string; // El nombre de la persona.
  edad: number;    // La edad de la persona.

  // Constructor:
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  // Métodos:
  saludar(): void {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

// Creamos un objeto de la clase "Persona".
const persona1 = new Persona("Juan", 25);

// Llamamos al método "saludar" del objeto "persona1".
persona1.saludar(); // Imprime: "Hola, mi nombre es Juan y tengo 25 años."

// Definimos una interfaz llamada "Animal" que define las características y comportamientos comunes de un animal.
interface Animal {
  nombre: string;
  especie: string;

  hablar(): void;
}

// Creamos una clase llamada "Perro" que implementa la interfaz "Animal".
class Perro implements Animal {
  nombre: string;
  especie: string = "Perro";

  constructor(nombre: string) {
    this.nombre = nombre;
  }

  hablar(): void {
    console.log("Woof!");
  }
}

// Creamos un objeto de la clase "Perro".
const perro1 = new Perro("Firulais");

// Llamamos al método "hablar" del objeto "perro1".
perro1.hablar(); // Imprime: "Woof!"

// Definimos una clase genérica llamada "Contenedor" que puede almacenar cualquier tipo de dato.
class Contenedor<T> {
  elementos: T[] = [];

  agregar(elemento: T): void {
    this.elementos.push(elemento);
  }

  obtener(indice: number): T {
    return this.elementos[indice];
  }
}

// Creamos un contenedor de números.
const contenedorNumeros = new Contenedor<number>();

// Agregamos algunos números al contenedor.
contenedorNumeros.agregar(1);
contenedorNumeros.agregar(2);
contenedorNumeros.agregar(3);

// Obtenemos el segundo elemento del contenedor.
const segundoNumero = contenedorNumeros.obtener(1);

console.log(`El segundo número es: ${segundoNumero}`); // Imprime: "El segundo número es: 2"

// Creamos un contenedor de cadenas de texto.
const contenedorCadenas = new Contenedor<string>();

// Agregamos algunas cadenas de texto al contenedor.
contenedorCadenas.agregar("Hola");
contenedorCadenas.agregar("Mundo");
contenedorCadenas.agregar("!");

// Obtenemos la primera cadena de texto del contenedor.
const primeraCadena = contenedorCadenas.obtener(0);

console.log(`La primera cadena es: ${primeraCadena}`); // Imprime: "La primera cadena es: Hola"
```

Explicación del código:

- El código define una clase llamada `Persona` que representa a una persona. La clase tiene dos propiedades: `nombre` y `edad`. También tiene un método llamado `saludar` que imprime un mensaje de saludo con el nombre y la edad de la persona.

- El código crea un objeto de la clase `Persona` llamado `persona1` y llama al método `saludar` del objeto. Esto imprime el mensaje de saludo en la consola.

- El código define una interfaz llamada `Animal` que representa a un animal. La interfaz tiene dos propiedades: `nombre` y `especie`. También tiene un método llamado `hablar` que imprime un mensaje de saludo para el animal.

- El código crea una clase llamada `Perro` que implementa la interfaz `Animal`. La clase `Perro` tiene una propiedad llamada `nombre` y una propiedad llamada `especie` con un valor predeterminado de "Perro". También tiene un método llamado `hablar` que imprime el mensaje "Woof!".

- El código crea un objeto de la clase `Perro` llamado `perro1` y llama al método `hablar` del objeto. Esto imprime el mensaje "Woof!" en la consola.

- El código define una clase genérica llamada `Contenedor` que puede almacenar cualquier tipo de dato. La clase tiene una propiedad llamada `elementos` que es un array vacío. También tiene dos métodos: `agregar` que agrega un elemento al array y `obtener` que devuelve el elemento en una posición específica del array.

- El código crea un contenedor de números llamado `contenedorNumeros` y le agrega los números 1, 2 y 3. Luego, obtiene el segundo elemento del contenedor y lo imprime en la consola.

- El código crea un contenedor de cadenas de texto llamado `contenedorCadenas` y le agrega las cadenas de texto "Hola", "Mundo" y "¡". Luego, obtiene la primera cadena de texto del contenedor y la imprime en la consola.