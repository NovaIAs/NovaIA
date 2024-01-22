```typescript
// Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(a: number, b: number): number {
  if (b == 0) {
    return a;
  } else {
    return mcd(b, a % b);
  }
}

// Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(a: number, b: number): number {
  return (a * b) / mcd(a, b);
}

// Función para generar una matriz de números primos hasta un límite dado
function generarPrimos(limite: number): number[] {
  let primos: number[] = [];
  for (let i = 2; i <= limite; i++) {
    let esPrimo = true;
    for (let j = 2; j <= i / 2; j++) {
      if (i % j == 0) {
        esPrimo = false;
        break;
      }
    }
    if (esPrimo) {
      primos.push(i);
    }
  }
  return primos;
}

// Función para encontrar el número de divisores de un número dado
function numDivisores(n: number): number {
  let divisores = 0;
  for (let i = 1; i <= n; i++) {
    if (n % i == 0) {
      divisores++;
    }
  }
  return divisores;
}

// Función para encontrar el número de divisores primos de un número dado
function numDivisoresPrimos(n: number): number {
  let divisoresPrimos = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      divisoresPrimos++;
    }
  }
  return divisoresPrimos;
}

// Función para encontrar el factor primo más grande de un número dado
function factorPrimoMasGrande(n: number): number {
  let factorPrimoMasGrande = 1;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      factorPrimoMasGrande = i;
    }
  }
  return factorPrimoMasGrande;
}

// Función para encontrar el número de factores primos distintos de un número dado
function numFactoresPrimosDistintos(n: number): number {
  let factoresPrimosDistintos = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      factoresPrimosDistintos++;
    }
  }
  return factoresPrimosDistintos;
}

// Función para encontrar el número de factores primos repetidos de un número dado
function numFactoresPrimosRepetidos(n: number): number {
  let factoresPrimosRepetidos = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) > 2) {
      factoresPrimosRepetidos++;
    }
  }
  return factoresPrimosRepetidos;
}

// Función para encontrar la suma de los divisores de un número dado
function sumaDivisores(n: number): number {
  let suma = 0;
  for (let i = 1; i <= n; i++) {
    if (n % i == 0) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los divisores primos de un número dado
function sumaDivisoresPrimos(n: number): number {
  let suma = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los factores primos distintos de un número dado
function sumaFactoresPrimosDistintos(n: number): number {
  let suma = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los factores primos repetidos de un número dado
function sumaFactoresPrimosRepetidos(n: number): number {
  let suma = 0;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) > 2) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar el producto de los divisores de un número dado
function productoDivisores(n: number): number {
  let producto = 1;
  for (let i = 1; i <= n; i++) {
    if (n % i == 0) {
      producto *= i;
    }
  }
  return producto;
}

// Función para encontrar el producto de los divisores primos de un número dado
function productoDivisoresPrimos(n: number): number {
  let producto = 1;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      producto *= i;
    }
  }
  return producto;
}

// Función para encontrar el producto de los factores primos distintos de un número dado
function productoFactoresPrimosDistintos(n: number): number {
  let producto = 1;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) == 2) {
      producto *= i;
    }
  }
  return producto;
}

// Función para encontrar el producto de los factores primos repetidos de un número dado
function productoFactoresPrimosRepetidos(n: number): number {
  let producto = 1;
  for (let i = 2; i <= n; i++) {
    if (n % i == 0 && numDivisores(i) > 2) {
      producto *= i;
    }
  }
  return producto;
}

// Función para encontrar el número de divisores comunes entre dos números dados
function numDivisoresComunes(a: number, b: number): number {
  let divisoresComunes = 0;
  for (let i = 1; i <= a; i++) {
    if (a % i == 0 && b % i == 0) {
      divisoresComunes++;
    }
  }
  return divisoresComunes;
}

// Función para encontrar el número de divisores comunes primos entre dos números dados
function numDivisoresComunesPrimos(a: number, b: number): number {
  let divisoresComunesPrimos = 0;
  for (let i = 2; i <= a; i++) {
    if (a % i == 0 && b % i == 0 && numDivisores(i) == 2) {
      divisoresComunesPrimos++;
    }
  }
  return divisoresComunesPrimos;
}

// Función para encontrar el número de divisores comunes distintos entre dos números dados
function numDivisoresComunesDistintos(a: number, b: number): number {
  let divisoresComunesDistintos = 0;
  for (let i = 2; i <= a; i++) {
    if (a % i == 0 && b % i == 0 && numDivisores(i) == 2) {
      divisoresComunesDistintos++;
    }
  }
  return divisoresComunesDistintos;
}

// Función para encontrar el número de divisores comunes repetidos entre dos números dados
function numDivisoresComunesRepetidos(a: number, b: number): number {
  let divisoresComunesRepetidos = 0;
  for (let i = 2; i <= a; i++) {
    if (a % i == 0 && b % i == 0 && numDivisores(i) > 2) {
      divisoresComunesRepetidos++;
    }
  }
  return divisoresComunesRepetidos;
}

// Función para encontrar la suma de los divisores comunes entre dos números dados
function sumaDivisoresComunes(a: number, b: number): number {
  let suma = 0;
  for (let i = 1; i <= a; i++) {
    if (a % i == 0 && b % i == 0) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los divisores comunes primos entre dos números dados
function sumaDivisoresComunesPrimos(a: number, b: number): number {
  let suma = 0;
  for (let i = 2; i <= a; i++) {
    if (a % i == 0 && b % i == 0 && numDivisores(i) == 2) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los divisores comunes distintos entre dos números dados
function sumaDivisoresComunesDistintos(a: number, b: number): number {
  let suma = 0;
  for (let i = 2; i <= a; i++) {
    if (a % i == 0 && b % i == 0 && numDivisores(i) == 2) {
      suma += i;
    }
  }
  return suma;
}

// Función para encontrar la suma de los divisores comunes repetidos entre dos números dados
function sumaDiv