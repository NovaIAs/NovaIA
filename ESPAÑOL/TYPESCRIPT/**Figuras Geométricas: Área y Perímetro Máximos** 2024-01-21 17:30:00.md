```typescript
// Definición de una clase abstracta para modelar figuras geométricas.
abstract class FiguraGeometrica {
  // Método abstracto para calcular el área de la figura.
  abstract calcularArea(): number;

  // Método abstracto para calcular el perímetro de la figura.
  abstract calcularPerimetro(): number;

  // Método para obtener el nombre de la figura.
  abstract obtenerNombre(): string;
}

// Definición de una clase que representa un círculo.
class Circulo extends FiguraGeometrica {
  // Atributo que representa el radio del círculo.
  private radio: number;

  // Constructor de la clase Círculo.
  constructor(radio: number) {
    super();
    this.radio = radio;
  }

  // Implementación del método para calcular el área del círculo.
  calcularArea(): number {
    return Math.PI * this.radio ** 2;
  }

  // Implementación del método para calcular el perímetro del círculo.
  calcularPerimetro(): number {
    return 2 * Math.PI * this.radio;
  }

  // Implementación del método para obtener el nombre de la figura.
  obtenerNombre(): string {
    return "Círculo";
  }
}

// Definición de una clase que representa un cuadrado.
class Cuadrado extends FiguraGeometrica {
  // Atributo que representa el lado del cuadrado.
  private lado: number;

  // Constructor de la clase Cuadrado.
  constructor(lado: number) {
    super();
    this.lado = lado;
  }

  // Implementación del método para calcular el área del cuadrado.
  calcularArea(): number {
    return this.lado ** 2;
  }

  // Implementación del método para calcular el perímetro del cuadrado.
  calcularPerimetro(): number {
    return 4 * this.lado;
  }

  // Implementación del método para obtener el nombre de la figura.
  obtenerNombre(): string {
    return "Cuadrado";
  }
}

// Definición de una clase que representa un triángulo.
class Triangulo extends FiguraGeometrica {
  // Atributos que representan los lados del triángulo.
  private lado1: number;
  private lado2: number;
  private lado3: number;

  // Constructor de la clase Triángulo.
  constructor(lado1: number, lado2: number, lado3: number) {
    super();
    this.lado1 = lado1;
    this.lado2 = lado2;
    this.lado3 = lado3;
  }

  // Implementación del método para calcular el área del triángulo.
  calcularArea(): number {
    // Fórmula de Heron para calcular el área de un triángulo.
    const semiperímetro = (this.lado1 + this.lado2 + this.lado3) / 2;
    return Math.sqrt(
      semiperímetro *
        (semiperímetro - this.lado1) *
        (semiperímetro - this.lado2) *
        (semiperímetro - this.lado3)
    );
  }

  // Implementación del método para calcular el perímetro del triángulo.
  calcularPerimetro(): number {
    return this.lado1 + this.lado2 + this.lado3;
  }

  // Implementación del método para obtener el nombre de la figura.
  obtenerNombre(): string {
    return "Triángulo";
  }
}

// Una función que recibe una lista de figuras geométricas y devuelve la figura con el área máxima.
function figuraConAreaMaxima(figuras: FiguraGeometrica[]): FiguraGeometrica {
  let figuraConAreaMaxima = figuras[0];
  for (let i = 1; i < figuras.length; i++) {
    if (figuras[i].calcularArea() > figuraConAreaMaxima.calcularArea()) {
      figuraConAreaMaxima = figuras[i];
    }
  }
  return figuraConAreaMaxima;
}

// Una función que recibe una lista de figuras geométricas y devuelve la figura con el perímetro máximo.
function figuraConPerimetroMaximo(figuras: FiguraGeometrica[]): FiguraGeometrica {
  let figuraConPerimetroMaximo = figuras[0];
  for (let i = 1; i < figuras.length; i++) {
    if (figuras[i].calcularPerimetro() > figuraConPerimetroMaximo.calcularPerimetro()) {
      figuraConPerimetroMaximo = figuras[i];
    }
  }
  return figuraConPerimetroMaximo;
}

// Creación de una lista de figuras geométricas.
const figuras: FiguraGeometrica[] = [
  new Circulo(5),
  new Cuadrado(10),
  new Triangulo(3, 4, 5),
];

// Impresión de la figura con el área máxima.
console.log("Figura con área máxima:");
console.log(figuraConAreaMaxima(figuras).obtenerNombre());

// Impresión de la figura con el perímetro máximo.
console.log("Figura con perímetro máximo:");
console.log(figuraConPerimetroMaximo(figuras).obtenerNombre());
```

Explicación del código:

* Se define una clase abstracta llamada `FiguraGeometrica` que representa una figura geométrica genérica. Esta clase tiene tres métodos abstractos: `calcularArea()`, `calcularPerimetro()` y `obtenerNombre()`.
* Se definen tres clases concretas que heredan de `FiguraGeometrica`: `Circulo`, `Cuadrado` y `Triangulo`. Estas clases implementan los métodos abstractos de la clase `FiguraGeometrica` para calcular el área, el perímetro y el nombre de cada figura específica.
* Se define una función llamada `figuraConAreaMaxima()` que recibe una lista de figuras geométricas y devuelve la figura con el área máxima. Esta función recorre la lista de figuras y devuelve la figura con el área más grande.
* Se define una función llamada `figuraConPerimetroMaximo()` que recibe una lista de figuras geométricas y devuelve la figura con el perímetro máximo. Esta función recorre la lista de figuras y devuelve la figura con el perímetro más grande.
* Se crea una lista de figuras geométricas que incluye un círculo, un cuadrado y un triángulo.
* Se imprime el nombre de la figura con el área máxima y el nombre de la figura con el perímetro máximo.