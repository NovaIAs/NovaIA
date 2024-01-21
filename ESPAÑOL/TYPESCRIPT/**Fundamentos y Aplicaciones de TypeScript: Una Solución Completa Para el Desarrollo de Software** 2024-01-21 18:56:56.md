```typescript
// Declaración de la clase "Persona" con propiedades y métodos
class Persona {
  nombre: string;
  edad: number;
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }
  hablar(): void {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

// Creación de un objeto de la clase "Persona"
const persona1 = new Persona("Juan", 25);

// Declaración de una función que recibe un objeto de la clase "Persona" y devuelve un string
function saludarPersona(persona: Persona): string {
  return `Hola, ${persona.nombre}!`;
}

// Declaración de una función genérica que recibe un array de cualquier tipo y devuelve un array del mismo tipo
function ordenarArray<T>(array: T[]): T[] {
  return array.sort();
}

// Uso de la función "ordenarArray" con un array de números
const numeros = [1, 3, 2, 4, 5];
const numerosOrdenados = ordenarArray(numeros);

// Uso de la función "ordenarArray" con un array de cadenas
const cadenas = ["hola", "mundo", "como", "estas"];
const cadenasOrdenadas = ordenarArray(cadenas);

// Declaración de un tipo de dato personalizado para representar un punto en el espacio
type Punto = {
  x: number;
  y: number;
};

// Declaración de una función que recibe un punto y devuelve la distancia al origen
function distanciaAlOrigen(punto: Punto): number {
  return Math.sqrt(punto.x ** 2 + punto.y ** 2);
}

// Uso de la función "distanciaAlOrigen" con un punto
const punto1 = { x: 3, y: 4 };
const distancia = distanciaAlOrigen(punto1);

// Declaración de una clase abstracta "Figura" con un método abstracto "calcularArea"
abstract class Figura {
  abstract calcularArea(): number;
}

// Declaración de una clase "Cuadrado" que hereda de la clase "Figura"
class Cuadrado extends Figura {
  lado: number;
  constructor(lado: number) {
    super();
    this.lado = lado;
  }
  calcularArea(): number {
    return this.lado ** 2;
  }
}

// Declaración de una clase "Triángulo" que hereda de la clase "Figura"
class Triángulo extends Figura {
  base: number;
  altura: number;
  constructor(base: number, altura: number) {
    super();
    this.base = base;
    this.altura = altura;
  }
  calcularArea(): number {
    return (this.base * this.altura) / 2;
  }
}

// Creación de un objeto de la clase "Cuadrado"
const cuadrado1 = new Cuadrado(5);

// Creación de un objeto de la clase "Triángulo"
const triángulo1 = new Triángulo(3, 4);

// Cálculo del área del cuadrado y del triángulo
const areaCuadrado = cuadrado1.calcularArea();
const areaTriángulo = triángulo1.calcularArea();
```

Explicación del código:

* Se declara una clase "Persona" con propiedades "nombre" y "edad" y un método "hablar".
* Se crea un objeto de la clase "Persona".
* Se declara una función que recibe un objeto de la clase "Persona" y devuelve un string.
* Se declara una función genérica que recibe un array de cualquier tipo y devuelve un array del mismo tipo.
* Se usa la función "ordenarArray" con un array de números y un array de cadenas.
* Se declara un tipo de dato personalizado para representar un punto en el espacio.
* Se declara una función que recibe un punto y devuelve la distancia al origen.
* Se usa la función "distanciaAlOrigen" con un punto.
* Se declara una clase abstracta "Figura" con un método abstracto "calcularArea".
* Se declaran dos clases "Cuadrado" y "Triángulo" que heredan de la clase "Figura" e implementan el método "calcularArea".
* Se crean objetos de las clases "Cuadrado" y "Triángulo".
* Se calcula el área del cuadrado y del triángulo.