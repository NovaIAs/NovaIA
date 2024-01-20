```typescript
// Definición de una clase abstracta para representar una figura geométrica.
abstract class FiguraGeometrica {
  // Propiedades abstractas para el área y el perímetro de la figura.
  abstract area: number;
  abstract perimetro: number;

  // Método abstracto para calcular el área de la figura.
  abstract calcularArea(): number;

  // Método abstracto para calcular el perímetro de la figura.
  abstract calcularPerimetro(): number;

  // Método para imprimir información sobre la figura geométrica.
  imprimirInformacion(): void {
    console.log(`Área: ${this.area}`);
    console.log(`Perímetro: ${this.perímetro}`);
  }
}

// Definición de una clase que representa un círculo.
class Circulo extends FiguraGeometrica {
  // Propiedad para el radio del círculo.
  radio: number;

  // Constructor para inicializar el círculo con un radio.
  constructor(radio: number) {
    super();
    this.radio = radio;
  }

  // Implementación del método abstracto para calcular el área del círculo.
  calcularArea(): number {
    return Math.PI * this.radio ** 2;
  }

  // Implementación del método abstracto para calcular el perímetro del círculo.
  calcularPerimetro(): number {
    return 2 * Math.PI * this.radio;
  }
}

// Definición de una clase que representa un rectángulo.
class Rectángulo extends FiguraGeometrica {
  // Propiedades para la base y la altura del rectángulo.
  base: number;
  altura: number;

  // Constructor para inicializar el rectángulo con una base y una altura.
  constructor(base: number, altura: number) {
    super();
    this.base = base;
    this.altura = altura;
  }

  // Implementación del método abstracto para calcular el área del rectángulo.
  calcularArea(): number {
    return this.base * this.altura;
  }

  // Implementación del método abstracto para calcular el perímetro del rectángulo.
  calcularPerimetro(): number {
    return 2 * (this.base + this.altura);
  }
}

// Definición de una clase que representa un triángulo.
class Triángulo extends FiguraGeometrica {
  // Propiedades para los lados del triángulo.
  ladoA: number;
  ladoB: number;
  ladoC: number;

  // Constructor para inicializar el triángulo con los lados.
  constructor(ladoA: number, ladoB: number, ladoC: number) {
    super();
    this.ladoA = ladoA;
    this.ladoB = ladoB;
    this.ladoC = ladoC;
  }

  // Implementación del método abstracto para calcular el área del triángulo.
  calcularArea(): number {
    // Cálculo del semiperímetro del triángulo.
    const semiperimetro = (this.ladoA + this.ladoB + this.ladoC) / 2;

    // Cálculo del área del triángulo utilizando la fórmula de Herón.
    return Math.sqrt(
      semiperimetro *
        (semiperimetro - this.ladoA) *
        (semiperimetro - this.ladoB) *
        (semiperimetro - this.ladoC)
    );
  }

  // Implementación del método abstracto para calcular el perímetro del triángulo.
  calcularPerimetro(): number {
    return this.ladoA + this.ladoB + this.ladoC;
  }
}

// Creación de objetos para representar un círculo, un rectángulo y un triángulo.
const circulo = new Circulo(5);
const rectángulo = new Rectángulo(10, 5);
const triángulo = new Triángulo(3, 4, 5);

// Impresión de información sobre cada figura geométrica.
console.log("Círculo:");
circulo.imprimirInformacion();
console.log();

console.log("Rectángulo:");
rectángulo.imprimirInformacion();
console.log();

console.log("Triángulo:");
triángulo.imprimirInformacion();
```

Explicación del código:

* Se define una clase abstracta llamada `FiguraGeométrica` que representa una figura geométrica general. Esta clase tiene propiedades abstractas para el área y el perímetro de la figura, y métodos abstractos para calcular el área y el perímetro.
* Se definen tres clases concretas que heredan de `FiguraGeométrica`: `Círculo`, `Rectángulo` y `Triángulo`. Cada una de estas clases representa una figura geométrica específica y proporciona implementaciones concretas para los métodos abstractos heredados de `FiguraGeométrica`.
* Se crean objetos para representar un círculo, un rectángulo y un triángulo utilizando las clases `Circulo`, `Rectángulo` y `Triángulo` respectivamente.
* Se imprime información sobre cada figura geométrica utilizando el método `imprimirInformacion()` definido en la clase `FiguraGeométrica`.

Este código demuestra el uso de clases abstractas e interfaces en TypeScript para crear una jerarquía de clases que representan diferentes tipos de figuras geométricas. También muestra cómo utilizar objetos de estas clases para calcular y mostrar información sobre sus áreas y perímetros.