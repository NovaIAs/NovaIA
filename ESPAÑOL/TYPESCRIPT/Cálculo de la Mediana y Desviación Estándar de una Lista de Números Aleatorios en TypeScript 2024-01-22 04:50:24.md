```typescript
// Función principal
function main(): void {
  // Crear una lista de números aleatorios
  const numerosAleatorios: number[] = [];
  for (let i = 0; i < 10; i++) {
    numerosAleatorios.push(Math.random() * 100);
  }

  // Función para ordenar la lista de números
  function ordenarNumeros(a: number, b: number): number {
    return a - b;
  }

  // Ordenar la lista de números
  numerosAleatorios.sort(ordenarNumeros);

  // Función para calcular la mediana de la lista de números
  function calcularMediana(numeros: number[]): number {
    // Comprobar si la lista está vacía
    if (numeros.length === 0) {
      throw new Error("La lista está vacía");
    }

    // Calcular el índice medio de la lista
    const indiceMedio = Math.floor(numeros.length / 2);

    // Comprobar si la lista tiene un número par de elementos
    if (numeros.length % 2 === 0) {
      // Si la lista tiene un número par de elementos, la mediana es la media de los dos elementos centrales
      return (numeros[indiceMedio] + numeros[indiceMedio - 1]) / 2;
    } else {
      // Si la lista tiene un número impar de elementos, la mediana es el elemento central
      return numeros[indiceMedio];
    }
  }

  // Calcular la mediana de la lista de números
  const mediana = calcularMediana(numerosAleatorios);

  // Función para calcular la desviación estándar de la lista de números
  function calcularDesviacionEstandar(numeros: number[]): number {
    // Comprobar si la lista está vacía
    if (numeros.length === 0) {
      throw new Error("La lista está vacía");
    }

    // Calcular la media de la lista
    const media = numeros.reduce((a, b) => a + b) / numeros.length;

    // Calcular la desviación estándar de la lista
    const desviacionEstandar = Math.sqrt(
      numeros.reduce((a, b) => a + Math.pow(b - media, 2), 0) / numeros.length
    );

    return desviacionEstandar;
  }

  // Calcular la desviación estándar de la lista de números
  const desviacionEstandar = calcularDesviacionEstandar(numerosAleatorios);

  // Mostrar los resultados
  console.log("Lista de números aleatorios:");
  console.log(numerosAleatorios);
  console.log("Mediana:");
  console.log(mediana);
  console.log("Desviación estándar:");
  console.log(desviacionEstandar);
}

// Llamar a la función principal
main();
```

Explicación del código:

1. La función `main()` es la función principal del programa.

2. La lista `numerosAleatorios` se crea con un bucle `for` que genera 10 números aleatorios entre 0 y 100.

3. La función `ordenarNumeros()` es una función de comparación que se utiliza para ordenar la lista de números.

4. La lista `numerosAleatorios` se ordena utilizando el método `sort()` con la función `ordenarNumeros()` como parámetro.

5. La función `calcularMediana()` calcula la mediana de una lista de números. La mediana es el valor medio de una lista de números ordenados.

6. La lista `numerosAleatorios` se ordena primero utilizando el método `sort()`.

7. El índice medio de la lista se calcula dividiendo la longitud de la lista por 2.

8. Si la lista tiene un número par de elementos, la mediana es la media de los dos elementos centrales.

9. Si la lista tiene un número impar de elementos, la mediana es el elemento central.

10. La función `calcularDesviacionEstandar()` calcula la desviación estándar de una lista de números. La desviación estándar es una medida de la variabilidad de una lista de números.

11. La lista `numerosAleatorios` se ordena primero utilizando el método `sort()`.

12. La media de la lista se calcula sumando todos los números de la lista y dividiendo el resultado por el número de elementos de la lista.

13. La desviación estándar se calcula calculando la raíz cuadrada de la varianza de la lista. La varianza es la media de las desviaciones cuadradas de los números de la lista con respecto a la media.

14. Los resultados se muestran en la consola utilizando el método `console.log()`.