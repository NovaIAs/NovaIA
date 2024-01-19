```typescript
// Crear una clase abstracta para definir la interfaz común de todas las formas
abstract class Forma {
  // Método abstracto para calcular el área de la forma
  abstract calcularArea(): number;
  
  // Método abstracto para calcular el perímetro de la forma
  abstract calcularPerimetro(): number;
  
  // Método para obtener el tipo de la forma
  abstract obtenerTipo(): string;
}

// Crear una clase heredada para representar un cuadrado
class Cuadrado extends Forma {
  // Propiedades para almacenar la longitud de un lado del cuadrado
  private lado: number;
  
  // Constructor para inicializar el cuadrado con la longitud del lado
  constructor(lado: number) {
    super();
    this.lado = lado;
  }

  // Implementar el método para calcular el área del cuadrado
  calcularArea(): number {
    return this.lado * this.lado;
  }

  // Implementar el método para calcular el perímetro del cuadrado
  calcularPerimetro(): number {
    return 4 * this.lado;
  }

  // Implementar el método para obtener el tipo de forma
  obtenerTipo(): string {
    return 'Cuadrado';
  }
}

// Crear una clase heredada para representar un círculo
class Circulo extends Forma {
  // Propiedades para almacenar el radio del círculo
  private radio: number;
  
  // Constructor para inicializar el círculo con el radio
  constructor(radio: number) {
    super();
    this.radio = radio;
  }

  // Implementar el método para calcular el área del círculo
  calcularArea(): number {
    return Math.PI * Math.pow(this.radio, 2);
  }

  // Implementar el método para calcular el perímetro del círculo
  calcularPerimetro(): number {
    return 2 * Math.PI * this.radio;
  }

  // Implementar el método para obtener el tipo de forma
  obtenerTipo(): string {
    return 'Círculo';
  }
}

// Crear una clase heredada para representar un triángulo equilátero
class TrianguloEquilatero extends Forma {
  // Propiedades para almacenar la longitud de un lado del triángulo
  private lado: number;
  
  // Constructor para inicializar el triángulo equilátero con la longitud del lado
  constructor(lado: number) {
    super();
    this.lado = lado;
  }

  // Implementar el método para calcular el área del triángulo equilátero
  calcularArea(): number {
    return Math.sqrt(3) / 4 * this.lado * this.lado;
  }

  // Implementar el método para calcular el perímetro del triángulo equilátero
  calcularPerimetro(): number {
    return 3 * this.lado;
  }

  // Implementar el método para obtener el tipo de forma
  obtenerTipo(): string {
    return 'Triángulo Equilátero';
  }
}

// Crear una función para calcular el área total de una lista de formas
function calcularAreaTotal(formas: Forma[]): number {
  // Inicializar el área total en 0
  let areaTotal = 0;

  // Iterar sobre la lista de formas
  for (const forma of formas) {
    // Sumar el área de la forma actual al área total
    areaTotal += forma.calcularArea();
  }

  // Devolver el área total
  return areaTotal;
}

// Crear una función para calcular el perímetro total de una lista de formas
function calcularPerimetroTotal(formas: Forma[]): number {
  // Inicializar el perímetro total en 0
  let perimetroTotal = 0;

  // Iterar sobre la lista de formas
  for (const forma of formas) {
    // Sumar el perímetro de la forma actual al perímetro total
    perimetroTotal += forma.calcularPerimetro();
  }

  // Devolver el perímetro total
  return perimetroTotal;
}


// Ejemplo de uso de las clases y funciones definidas

// Crear una lista de formas
const formas: Forma[] = [
  new Cuadrado(5),
  new Circulo(3),
  new TrianguloEquilatero(4)
];

// Calcular el área total de la lista de formas
const areaTotal = calcularAreaTotal(formas);

// Calcular el perímetro total de la lista de formas
const perimetroTotal = calcularPerimetroTotal(formas);

// Mostrar el área total y el perímetro total
console.log(`Área total: ${areaTotal}`);
console.log(`Perímetro total: ${perimetroTotal}`);
```

Explicación del código:

1. Se definen las clases `Forma`, `Cuadrado`, `Circulo` y `TrianguloEquilatero`. Estas clases representan las diferentes formas geométricas que se pueden utilizar en el programa.

2. La clase `Forma` es una clase abstracta que define la interfaz común de todas las formas. Esta clase contiene los métodos abstractos `calcularArea()`, `calcularPerimetro()` y `obtenerTipo()`, que deben ser implementados por las clases heredadas.

3. Las clases `Cuadrado`, `Circulo` y `TrianguloEquilatero` son clases heredadas que implementan los métodos abstractos de la clase `Forma`. Cada clase tiene sus propias propiedades y métodos específicos para calcular el área y el perímetro de la forma correspondiente.

4. Se definen las funciones `calcularAreaTotal()` y `calcularPerimetroTotal()` para calcular el área total y el perímetro total de una lista de formas, respectivamente.

5. Se crea una lista de formas que contiene un cuadrado, un círculo y un triángulo equilátero.

6. Se llama a las funciones `calcularAreaTotal()` y `calcularPerimetroTotal()` para calcular el área total y el perímetro total de la lista de formas.

7. Se muestran los resultados en la consola.