```javascript
// 1. Definir una función para calcular el factorial de un número

function factorial(numero) {
  if (numero < 0) {
    throw new Error("El número debe ser mayor o igual a 0");
  }

  if (numero === 0) {
    return 1;
  }

  let resultado = 1;
  for (let i = 1; i <= numero; i++) {
    resultado *= i;
  }

  return resultado;
}

// 2. Definir una función para calcular la combinación de n elementos tomados de r en r

function combinacion(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  if (r > n) {
    throw new Error("El número de elementos tomados debe ser menor o igual al número de elementos disponibles");
  }

  let resultado = factorial(n) / (factorial(r) * factorial(n - r));

  return resultado;
}

// 3. Definir una función para calcular la permutación de n elementos tomados de r en r

function permutacion(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  if (r > n) {
    throw new Error("El número de elementos tomados debe ser menor o igual al número de elementos disponibles");
  }

  let resultado = factorial(n) / factorial(n - r);

  return resultado;
}

// 4. Definir una función para calcular el coeficiente binomial de n elementos tomados de r en r

function coeficienteBinomial(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  if (r > n) {
    throw new Error("El número de elementos tomados debe ser menor o igual al número de elementos disponibles");
  }

  let resultado = combinacion(n, r) / factorial(r);

  return resultado;
}

// 5. Definir una función para calcular el número de permutaciones de n elementos con repetición

function permutacionesConRepeticion(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  let resultado = Math.pow(n, r);

  return resultado;
}

// 6. Definir una función para calcular el número de combinaciones de n elementos con repetición

function combinacionesConRepeticion(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  let resultado = Math.pow(n + r - 1, r) / factorial(r);

  return resultado;
}

// 7. Definir una función para calcular el número de permutaciones cíclicas de n elementos

function permutacionesCiclicas(n) {
  if (n < 0) {
    throw new Error("El número debe ser mayor o igual a 0");
  }

  let resultado = factorial(n) / n;

  return resultado;
}

// 8. Definir una función para calcular el número de combinaciones cíclicas de n elementos

function combinacionesCiclicas(n) {
  if (n < 0) {
    throw new Error("El número debe ser mayor o igual a 0");
  }

  let resultado = (factorial(n) / n) / 2;

  return resultado;
}

// 9. Definir una función para calcular el número de permutaciones con elementos distintos y sin repetición

function permutacionesSinRepeticion(n) {
  if (n < 0) {
    throw new Error("El número debe ser mayor o igual a 0");
  }

  let resultado = factorial(n);

  return resultado;
}

// 10. Definir una función para calcular el número de combinaciones con elementos distintos y sin repetición

function combinacionesSinRepeticion(n, r) {
  if (n < 0 || r < 0) {
    throw new Error("Los números deben ser mayores o iguales a 0");
  }

  if (r > n) {
    throw new Error("El número de elementos tomados debe ser menor o igual al número de elementos disponibles");
  }

  let resultado = factorial(n) / (factorial(r) * factorial(n - r));

  return resultado;
}
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `combinacion` calcula la combinación de n elementos tomados de r en r.
* La función `permutacion` calcula la permutación de n elementos tomados de r en r.
* La función `coeficienteBinomial` calcula el coeficiente binomial de n elementos tomados de r en r.
* La función `permutacionesConRepeticion` calcula el número de permutaciones de n elementos con repetición.
* La función `combinacionesConRepeticion` calcula el número de combinaciones de n elementos con repetición.
* La función `permutacionesCiclicas` calcula el número de permutaciones cíclicas de n elementos.
* La función `combinacionesCiclicas` calcula el número de combinaciones cíclicas de n elementos.
* La función `permutacionesSinRepeticion` calcula el número de permutaciones con elementos distintos y sin repetición.
* La función `combinacionesSinRepeticion` calcula el número de combinaciones con elementos distintos y sin repetición.

Este código es una colección de funciones matemáticas que pueden ser utilizadas para resolver una variedad de problemas. Las funciones están escritas en JavaScript y son fáciles de usar.