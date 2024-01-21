```javascript
// Crear una función para calcular el factorial de un número

function factorial(numero) {
  // Si el número es menor que 0, devolver 1
  if (numero < 0) {
    return 1;
  }

  // Si el número es igual a 0, devolver 1
  if (numero === 0) {
    return 1;
  }

  // Si el número es mayor que 0, calcular el factorial
  let factorial = 1;
  for (let i = 1; i <= numero; i++) {
    factorial *= i;
  }

  // Devolver el factorial
  return factorial;
}

// Crear una función para calcular el número de combinaciones de n elementos tomados de m en m

function combinaciones(n, m) {
  // Si n es menor que 0 o m es menor que 0, devolver 0
  if (n < 0 || m < 0) {
    return 0;
  }

  // Si n es igual a m, devolver 1
  if (n === m) {
    return 1;
  }

  // Si m es igual a 0, devolver 1
  if (m === 0) {
    return 1;
  }

  // Si n es mayor que m, calcular el número de combinaciones
  let combinaciones = factorial(n) / (factorial(m) * factorial(n - m));

  // Devolver el número de combinaciones
  return combinaciones;
}

// Crear una función para calcular el número de permutaciones de n elementos tomados de m en m

function permutaciones(n, m) {
  // Si n es menor que 0 o m es menor que 0, devolver 0
  if (n < 0 || m < 0) {
    return 0;
  }

  // Si n es igual a m, devolver 1
  if (n === m) {
    return 1;
  }

  // Si m es igual a 0, devolver 1
  if (m === 0) {
    return 1;
  }

  // Si n es mayor que m, calcular el número de permutaciones
  let permutaciones = factorial(n) / factorial(n - m);

  // Devolver el número de permutaciones
  return permutaciones;
}

// Crear una función para calcular el máximo común divisor de dos números

function maximoComunDivisor(a, b) {
  // Si a es menor que 0 o b es menor que 0, devolver 0
  if (a < 0 || b < 0) {
    return 0;
  }

  // Si a es igual a 0, devolver b
  if (a === 0) {
    return b;
  }

  // Si b es igual a 0, devolver a
  if (b === 0) {
    return a;
  }

  // Si a es mayor que 0 y b es mayor que 0, calcular el máximo común divisor
  let maximoComunDivisor = 1;
  for (let i = 1; i <= Math.min(a, b); i++) {
    if (a % i === 0 && b % i === 0) {
      maximoComunDivisor = i;
    }
  }

  // Devolver el máximo común divisor
  return maximoComunDivisor;
}

// Crear una función para calcular el mínimo común múltiplo de dos números

function minimoComunMultiplo(a, b) {
  // Si a es menor que 0 o b es menor que 0, devolver 0
  if (a < 0 || b < 0) {
    return 0;
  }

  // Si a es igual a 0 o b es igual a 0, devolver 0
  if (a === 0 || b === 0) {
    return 0;
  }

  // Calcular el máximo común divisor de a y b
  let maximoComunDivisor = maximoComunDivisor(a, b);

  // Calcular el mínimo común múltiplo de a y b
  let minimoComunMultiplo = (a * b) / maximoComunDivisor;

  // Devolver el mínimo común múltiplo
  return minimoComunMultiplo;
}

// Crear una función para calcular el número de formas de elegir k elementos de un conjunto de n elementos, sin importar el orden

function combinacionesSinOrden(n, k) {
  // Si n es menor que 0 o k es menor que 0, devolver 0
  if (n < 0 || k < 0) {
    return 0;
  }

  // Si n es igual a k, devolver 1
  if (n === k) {
    return 1;
  }

  // Si k es igual a 0, devolver 1
  if (k === 0) {
    return 1;
  }

  // Si n es mayor que k, calcular el número de formas de elegir k elementos de un conjunto de n elementos, sin importar el orden
  let combinacionesSinOrden = combinaciones(n - 1, k - 1);

  // Devolver el número de formas de elegir k elementos de un conjunto de n elementos, sin importar el orden
  return combinacionesSinOrden;
}

// Crear una función para calcular el número de formas de elegir k elementos de un conjunto de n elementos, teniendo en cuenta el orden

function permutacionesConOrden(n, k) {
  // Si n es menor que 0 o k es menor que 0, devolver 0
  if (n < 0 || k < 0) {
    return 0;
  }

  // Si n es igual a k, devolver 1
  if (n === k) {
    return 1;
  }

  // Si k es igual a 0, devolver 1
  if (k === 0) {
    return 1;
  }

  // Si n es mayor que k, calcular el número de formas de elegir k elementos de un conjunto de n elementos, teniendo en cuenta el orden
  let permutacionesConOrden = permutaciones(n - 1, k - 1) * n;

  // Devolver el número de formas de elegir k elementos de un conjunto de n elementos, teniendo en cuenta el orden
  return permutacionesConOrden;
}
```

Este código es un conjunto de funciones matemáticas en JavaScript. Incluye funciones para calcular el factorial de un número, el número de combinaciones de n elementos tomados de m en m, el número de permutaciones de n elementos tomados de m en m, el máximo común divisor de dos números, el mínimo común múltiplo de dos números, el número de formas de elegir k elementos de un conjunto de n elementos, sin importar el orden, y el número de formas de elegir k elementos de un conjunto de n elementos, teniendo en cuenta el orden.

Las funciones están escritas en JavaScript y están documentadas con comentarios. El código está bien organizado y es fácil de leer.