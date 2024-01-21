```javascript
// Definimos una función asíncrona que recibe un array de números.
const sumaAsincrona = async (numeros) => {
  // Devolvemos una promesa que se resolverá cuando se haya completado la suma de todos los números.
  return new Promise((resolve, reject) => {
    // Si el array de números está vacío, rechazamos la promesa con un error.
    if (numeros.length === 0) {
      reject(new Error("El array de números está vacío."));
    }
    // Si el array de números no está vacío, calculamos la suma de todos los números.
    else {
      let suma = 0;
      for (const numero of numeros) {
        suma += numero;
      }
      // Resolvemos la promesa con la suma de los números.
      resolve(suma);
    }
  });
};

// Definimos una función que recibe un array de números y los muestra por consola.
const mostrarNumeros = (numeros) => {
  // Recorremos el array de números y los mostramos por consola.
  for (const numero of numeros) {
    console.log(numero);
  }
};

// Definimos una función que recibe un array de números y los filtra.
const filtrarNumeros = (numeros) => {
  // Devolvemos un nuevo array con los números del array original que sean mayores que 10.
  return numeros.filter((numero) => {
    return numero > 10;
  });
};

// Definimos una función que recibe un array de números y los ordena.
const ordenarNumeros = (numeros) => {
  // Devolvemos un nuevo array con los números del array original ordenados de menor a mayor.
  return numeros.sort((a, b) => {
    return a - b;
  });
};

// Definimos una función que recibe un array de números y devuelve el máximo de ellos.
const maximoNumero = (numeros) => {
  // Devolvemos el valor máximo del array de números.
  return Math.max(...numeros);
};

// Definimos una función que recibe un array de números y devuelve el mínimo de ellos.
const minimoNumero = (numeros) => {
  // Devolvemos el valor mínimo del array de números.
  return Math.min(...numeros);
};

// Definimos una función que recibe un array de números y devuelve su media.
const mediaNumeros = (numeros) => {
  // Calculamos la suma de todos los números del array.
  const suma = numeros.reduce((a, b) => {
    return a + b;
  });
  // Devolvemos la suma de los números dividida entre el número de elementos del array.
  return suma / numeros.length;
};

// Definimos una función que recibe un array de números y devuelve su mediana.
const medianaNumeros = (numeros) => {
  // Ordenamos el array de números de menor a mayor.
  numeros.sort((a, b) => {
    return a - b;
  });
  // Si el array de números tiene un número impar de elementos, devolvemos el valor del elemento central.
  if (numeros.length % 2 === 1) {
    return numeros[Math.floor(numeros.length / 2)];
  }
  // Si el array de números tiene un número par de elementos, devolvemos la media de los dos elementos centrales.
  else {
    return (numeros[Math.floor(numeros.length / 2 - 1)] + numeros[Math.floor(numeros.length / 2)]) / 2;
  }
};

// Definimos una función que recibe un array de números y devuelve su moda.
const modaNumeros = (numeros) => {
  // Creamos un objeto para almacenar la frecuencia de cada número del array.
  const frecuencias = {};
  for (const numero of numeros) {
    frecuencias[numero] = (frecuencias[numero] || 0) + 1;
  }
  // Obtenemos el número con la mayor frecuencia.
  let moda = null;
  let maxFrecuencia = 0;
  for (const numero in frecuencias) {
    if (frecuencias[numero] > maxFrecuencia) {
      moda = numero;
      maxFrecuencia = frecuencias[numero];
    }
  }
  // Devolvemos el número con la mayor frecuencia.
  return moda;
};

// Definimos una función que recibe un array de números y devuelve su desviación estándar.
const desviacionEstandarNumeros = (numeros) => {
  // Calculamos la media de los números del array.
  const media = numeros.reduce((a, b) => {
    return a + b;
  }) / numeros.length;
  // Calculamos la suma de las desviaciones cuadradas de los números respecto a la media.
  const desviacionesCuadradas = numeros.reduce((a, b) => {
    return a + Math.pow(b - media, 2);
  }, 0);
  // Devolvemos la raíz cuadrada de la suma de las desviaciones cuadradas dividida entre el número de elementos del array.
  return Math.sqrt(desviacionesCuadradas / numeros.length);
};

// Definimos una función que recibe un array de números y devuelve su coeficiente de variación.
const coeficienteVariacionNumeros = (numeros) => {
  // Calculamos la desviación estándar de los números del array.
  const desviacionEstandar = desviacionEstandarNumeros(numeros);
  // Calculamos la media de los números del array.
  const media = numeros.reduce((a, b) => {
    return a + b;
  }) / numeros.length;
  // Devolvemos la desviación estándar dividida entre la media.
  return desviacionEstandar / media;
};
```

Explicación del código:

- La función `sumaAsincrona` recibe un array de números y devuelve una promesa que se resolverá cuando se haya completado la suma de todos los números.
- La función `mostrarNumeros` recibe un array de números y los muestra por consola.
- La función `filtrarNumeros` recibe un array de números y los filtra.
- La función `ordenarNumeros` recibe un array de números y los ordena.
- La función `maximoNumero` recibe un array de números y devuelve el máximo de ellos.
- La función `minimoNumero` recibe un array de números y devuelve el mínimo de ellos.
- La función `mediaNumeros` recibe un array de números y devuelve su media.
- La función `medianaNumeros` recibe un array de números y devuelve su mediana.
- La función `modaNumeros` recibe un array de números y devuelve su moda.
- La función `desviacionEstandarNumeros` recibe un array de números y devuelve su desviación estándar.
- La función `coeficienteVariacionNumeros` recibe un array de números y devuelve su coeficiente de variación.