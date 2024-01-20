```javascript
// Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(a, b) {
  if (b === 0) {
    return a;
  }
  return mcd(b, a % b);
}

// Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(a, b) {
  return (a * b) / mcd(a, b);
}

// Función para generar una lista de números primos hasta un límite dado
function primos(limite) {
  const numeros = [];
  for (let i = 2; i <= limite; i++) {
    let esPrimo = true;
    for (let j = 2; j <= Math.sqrt(i); j++) {
      if (i % j === 0) {
        esPrimo = false;
        break;
      }
    }
    if (esPrimo) {
      numeros.push(i);
    }
  }
  return numeros;
}

// Función para verificar si un número es primo
function esPrimo(numero) {
  if (numero <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      return false;
    }
  }
  return true;
}

// Función para encontrar la factorización prima de un número
function factorizacionPrima(numero) {
  const factores = [];
  let divisor = 2;
  while (numero > 1) {
    if (numero % divisor === 0) {
      factores.push(divisor);
      numero /= divisor;
    } else {
      divisor++;
    }
  }
  return factores;
}

// Función para encontrar el número de divisores de un número
function numDivisores(numero) {
  let divisores = 0;
  for (let i = 1; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      divisores++;
      if (i !== numero / i) {
        divisores++;
      }
    }
  }
  return divisores;
}

// Función para encontrar la suma de los divisores de un número
function sumaDivisores(numero) {
  let suma = 0;
  for (let i = 1; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      suma += i;
      if (i !== numero / i) {
        suma += numero / i;
      }
    }
  }
  return suma;
}

// Función para encontrar el número perfecto más cercano a un número dado
function perfectoCercano(numero) {
  let candidato = numero;
  while (!esPerfecto(candidato)) {
    candidato++;
  }
  return candidato;
}

// Función para verificar si un número es perfecto
function esPerfecto(numero) {
  let suma = 0;
  for (let i = 1; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      suma += i;
      if (i !== numero / i) {
        suma += numero / i;
      }
    }
  }
  return suma === 2 * numero;
}

// Función para encontrar el número de Fibonacci en una posición dada
function fibonacci(posicion) {
  if (posicion <= 1) {
    return posicion;
  }
  return fibonacci(posicion - 1) + fibonacci(posicion - 2);
}

// Función para encontrar la serie de Fibonacci hasta un número dado de términos
function fibonacciSerie(limite) {
  const serie = [];
  for (let i = 0; i < limite; i++) {
    serie.push(fibonacci(i));
  }
  return serie;
}
```

Este código contiene varias funciones matemáticas útiles, incluyendo funciones para encontrar el MCD, MCM, factores primos, divisores y números perfectos. También incluye funciones para generar la serie de Fibonacci y verificar si un número es primo o perfecto.