```javascript
// Función que recibe una matriz y devuelve la suma de todos sus elementos.
function sumaElementos(matriz) {
  let suma = 0;
  for (let i = 0; i < matriz.length; i++) {
    suma += matriz[i];
  }
  return suma;
}

// Función que recibe una matriz de números y devuelve el promedio de todos sus elementos.
function promedioElementos(matriz) {
  let suma = sumaElementos(matriz);
  return suma / matriz.length;
}

// Función que recibe una matriz de números y devuelve la desviación estándar de todos sus elementos.
function desviacionEstandar(matriz) {
  let promedio = promedioElementos(matriz);
  let varianza = 0;
  for (let i = 0; i < matriz.length; i++) {
    varianza += Math.pow(matriz[i] - promedio, 2);
  }
  varianza /= matriz.length;
  return Math.sqrt(varianza);
}

// Función que recibe una matriz de números y devuelve la mediana de todos sus elementos.
function medianaElementos(matriz) {
  matriz.sort((a, b) => a - b); // Ordenamos la matriz de menor a mayor.
  let mitad = Math.floor(matriz.length / 2); // Calculamos la posición de la mitad de la matriz.
  if (matriz.length % 2 === 0) { // Si la matriz tiene un número par de elementos, devolvemos el promedio de los dos elementos centrales.
    return (matriz[mitad] + matriz[mitad - 1]) / 2;
  } else { // Si la matriz tiene un número impar de elementos, devolvemos el elemento central.
    return matriz[mitad];
  }
}

// Función que recibe una matriz de números y devuelve la moda de todos sus elementos.
function modaElementos(matriz) {
  let moda = [];
  let maximo = 0;
  let repeticiones = {};
  for (let i = 0; i < matriz.length; i++) {
    if (repeticiones[matriz[i]] === undefined) {
      repeticiones[matriz[i]] = 1;
    } else {
      repeticiones[matriz[i]]++;
    }
    if (repeticiones[matriz[i]] > maximo) {
      maximo = repeticiones[matriz[i]];
      moda = [matriz[i]];
    } else if (repeticiones[matriz[i]] === maximo) {
      moda.push(matriz[i]);
    }
  }
  return moda;
}

// Ejemplo de uso de las funciones.
const matriz = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
console.log("Suma de los elementos:", sumaElementos(matriz));
console.log("Promedio de los elementos:", promedioElementos(matriz));
console.log("Desviación estándar de los elementos:", desviacionEstandar(matriz));
console.log("Mediana de los elementos:", medianaElementos(matriz));
console.log("Moda de los elementos:", modaElementos(matriz));
```

Explicación del código:

* La función `sumaElementos` recorre la matriz y suma todos sus elementos, devolviendo la suma total.
* La función `promedioElementos` llama a `sumaElementos` para obtener la suma de los elementos de la matriz, y luego divide la suma por la longitud de la matriz para obtener el promedio.
* La función `desviacionEstandar` calcula primero el promedio de los elementos de la matriz, luego recorre la matriz y calcula la varianza, que es la suma de los cuadrados de las diferencias entre cada elemento y el promedio. La varianza se divide por la longitud de la matriz y se calcula la raíz cuadrada de la varianza para obtener la desviación estándar.
* La función `medianaElementos` ordena la matriz de menor a mayor y, si la matriz tiene un número par de elementos, devuelve el promedio de los dos elementos centrales. Si la matriz tiene un número impar de elementos, devuelve el elemento central.
* La función `modaElementos` recorre la matriz y cuenta el número de veces que aparece cada elemento. El elemento o elementos que aparecen el mayor número de veces se consideran la moda de la matriz.