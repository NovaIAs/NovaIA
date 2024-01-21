```typescript
// Clase base para definir operaciones matemáticas básicas
class OperacionesMatematicas {
  sumar(a: number, b: number): number {
    return a + b;
  }

  restar(a: number, b: number): number {
    return a - b;
  }

  multiplicar(a: number, b: number): number {
    return a * b;
  }

  dividir(a: number, b: number): number {
    if (b === 0) {
      throw new Error("No se puede dividir por cero.");
    }
    return a / b;
  }
}

// Clase que hereda de OperacionesMatematicas y agrega operaciones avanzadas
class CalculadoraAvanzada extends OperacionesMatematicas {
  potencia(a: number, b: number): number {
    return Math.pow(a, b);
  }

  raizCuadrada(a: number): number {
    if (a < 0) {
      throw new Error("No se puede calcular la raíz cuadrada de un número negativo.");
    }
    return Math.sqrt(a);
  }

  factorial(n: number): number {
    if (n < 0) {
      throw new Error("No se puede calcular el factorial de un número negativo.");
    }
    let factorial = 1;
    for (let i = 1; i <= n; i++) {
      factorial *= i;
    }
    return factorial;
  }
}

// Función principal que utiliza la clase CalculadoraAvanzada
function main() {
  // Creamos una instancia de la clase CalculadoraAvanzada
  const calculadora = new CalculadoraAvanzada();

  // Utilizamos los métodos de la clase para realizar operaciones matemáticas
  const suma = calculadora.sumar(10, 20);
  const resta = calculadora.restar(20, 10);
  const multiplicacion = calculadora.multiplicar(10, 20);
  const division = calculadora.dividir(20, 10);
  const potencia = calculadora.potencia(2, 10);
  const raizCuadrada = calculadora.raizCuadrada(25);
  const factorial = calculadora.factorial(5);

  // Mostramos los resultados en la consola
  console.log("Suma:", suma);
  console.log("Resta:", resta);
  console.log("Multiplicación:", multiplicacion);
  console.log("División:", division);
  console.log("Potencia:", potencia);
  console.log("Raíz cuadrada:", raizCuadrada);
  console.log("Factorial:", factorial);
}

// Llamamos a la función principal para ejecutar el programa
main();
```

Explicación del código:

1. La clase `OperacionesMatematicas` define las operaciones matemáticas básicas como sumar, restar, multiplicar y dividir.
2. La clase `CalculadoraAvanzada` hereda de `OperacionesMatematicas` y agrega operaciones avanzadas como potencia, raíz cuadrada y factorial.
3. La función `main` crea una instancia de `CalculadoraAvanzada` y utiliza sus métodos para realizar operaciones matemáticas.
4. Al final de la función, se muestran los resultados de las operaciones en la consola.

El código utiliza encapsulamiento, herencia y polimorfismo, que son conceptos fundamentales de la programación orientada a objetos.