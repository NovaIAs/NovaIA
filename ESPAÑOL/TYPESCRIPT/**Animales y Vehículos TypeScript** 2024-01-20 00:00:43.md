```typescript
// Definición de una clase abstracta "Animal"
abstract class Animal {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  abstract hacerRuido(): void;
}

// Definición de una clase concreta "Perro" que hereda de "Animal"
class Perro extends Animal {
  raza: string;

  constructor(nombre: string, edad: number, raza: string) {
    super(nombre, edad);
    this.raza = raza;
  }

  hacerRuido(): void {
    console.log(`${this.nombre}: ¡Guau, guau!`);
  }
}

// Definición de una clase concreta "Gato" que hereda de "Animal"
class Gato extends Animal {
  tipoPelo: string;

  constructor(nombre: string, edad: number, tipoPelo: string) {
    super(nombre, edad);
    this.tipoPelo = tipoPelo;
  }

  hacerRuido(): void {
    console.log(`${this.nombre}: ¡Miau, miau!`);
  }
}

// Definición de una interfaz "IVehiculo"
interface IVehiculo {
  marca: string;
  modelo: string;
  año: number;

  acelerar(): void;
  frenar(): void;
}

// Definición de una clase concreta "Coche" que implementa "IVehiculo"
class Coche implements IVehiculo {
  marca: string;
  modelo: string;
  año: number;

  constructor(marca: string, modelo: string, año: number) {
    this.marca = marca;
    this.modelo = modelo;
    this.año = año;
  }

  acelerar(): void {
    console.log(`${this.marca} ${this.modelo}: ¡Acelerando!`);
  }

  frenar(): void {
    console.log(`${this.marca} ${this.modelo}: ¡Frenando!`);
  }
}

// Definición de una clase concreta "Motocicleta" que implementa "IVehiculo"
class Motocicleta implements IVehiculo {
  marca: string;
  modelo: string;
  año: number;

  constructor(marca: string, modelo: string, año: number) {
    this.marca = marca;
    this.modelo = modelo;
    this.año = año;
  }

  acelerar(): void {
    console.log(`${this.marca} ${this.modelo}: ¡Acelerando!`);
  }

  frenar(): void {
    console.log(`${this.marca} ${this.modelo}: ¡Frenando!`);
  }
}

// Creación de instancias de las clases definidas
const perro = new Perro("Firulais", 5, "Labrador");
const gato = new Gato("Misifu", 3, "Pelo corto");

const coche = new Coche("Toyota", "Yaris", 2020);
const moto = new Motocicleta("Harley-Davidson", "Sportster", 2018);

// Llamada a los métodos de las clases creadas
perro.hacerRuido(); // Imprime "Firulais: ¡Guau, guau!"
gato.hacerRuido(); // Imprime "Misifu: ¡Miau, miau!"

coche.acelerar(); // Imprime "Toyota Yaris: ¡Acelerando!"
coche.frenar(); // Imprime "Toyota Yaris: ¡Frenando!"

moto.acelerar(); // Imprime "Harley-Davidson Sportster: ¡Acelerando!"
moto.frenar(); // Imprime "Harley-Davidson Sportster: ¡Frenando!"
```

Explicación del código:

1. **Clases Abstractas:**
   - Definimos una clase abstracta `Animal` que contiene los atributos comunes a todos los animales (nombre y edad) y un método abstracto `hacerRuido()`.
   - Las clases concretas `Perro` y `Gato` heredan de la clase abstracta `Animal` y proporcionan implementaciones específicas para los métodos abstractos.

2. **Interfaces:**
   - Definimos una interfaz `IVehiculo` que contiene los atributos y métodos comunes a todos los vehículos (marca, modelo y año).
   - Las clases concretas `Coche` y `Motocicleta` implementan la interfaz `IVehiculo` y proporcionan implementaciones específicas para los métodos de la interfaz.

3. **Creación de Instancias:**
   - Creamos instancias de las clases definidas (`perro`, `gato`, `coche`, `moto`) para poder utilizarlas en nuestro programa.

4. **Llamadas a Métodos:**
   - Llamamos a los métodos de las clases creadas para ver el resultado en la consola.