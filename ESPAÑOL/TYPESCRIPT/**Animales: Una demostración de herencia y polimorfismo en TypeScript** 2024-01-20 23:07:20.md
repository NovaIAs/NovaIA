```typescript
// Clase abstracta que representa un animal.
abstract class Animal {
  protected nombre: string;
  protected edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  // Método abstracto que debe ser implementado por las clases hijas.
  public abstract hacerRuido(): string;

  // Método concreto que puede ser utilizado por las clases hijas.
  public saludar(): string {
    return `Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`;
  }
}

// Clase concreta que representa un perro.
class Perro extends Animal {
  constructor(nombre: string, edad: number) {
    super(nombre, edad);
  }

  public hacerRuido(): string {
    return "Guau, guau!";
  }
}

// Clase concreta que representa un gato.
class Gato extends Animal {
  constructor(nombre: string, edad: number) {
    super(nombre, edad);
  }

  public hacerRuido(): string {
    return "Miau, miau!";
  }
}

// Clase concreta que representa una vaca.
class Vaca extends Animal {
  constructor(nombre: string, edad: number) {
    super(nombre, edad);
  }

  public hacerRuido(): string {
    return "Muu, muu!";
  }
}

// Función principal que crea objetos de las clases Perro, Gato y Vaca y llama a sus métodos.
function main(): void {
  const perro = new Perro("Firulais", 5);
  const gato = new Gato("Michi", 3);
  const vaca = new Vaca("Bessie", 10);

  console.log(perro.saludar());
  console.log(perro.hacerRuido());

  console.log(gato.saludar());
  console.log(gato.hacerRuido());

  console.log(vaca.saludar());
  console.log(vaca.hacerRuido());
}

// Llamada a la función principal.
main();
```

Explicación del código:

* La clase abstracta `Animal` define las propiedades y métodos comunes a todos los animales. En este caso, define las propiedades `nombre` y `edad`, y el método `hacerRuido()` que debe ser implementado por las clases hijas.
* Las clases concretas `Perro`, `Gato` y `Vaca` extienden de la clase `Animal` y proporcionan implementaciones específicas para el método `hacerRuido()`.
* La función `main()` crea objetos de las clases `Perro`, `Gato` y `Vaca`, y llama a sus métodos `saludar()` y `hacerRuido()`.

Este código es complejo en el sentido de que utiliza conceptos avanzados de TypeScript como clases abstractas, clases concretas, herencia y polimorfismo. Sin embargo, el código está bien estructurado y comentado, lo que lo hace fácil de entender y mantener. Es poco probable que este código se repita exactamente en otro lugar, ya que es una implementación específica de un problema particular.