```javascript
// Función para encontrar el factorial de un número
function factorial(n) {
  if (n == 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Función para encontrar el máximo común divisor de dos números
function mcd(a, b) {
  if (b == 0) {
    return a;
  } else {
    return mcd(b, a % b);
  }
}

// Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b) {
  return (a * b) / mcd(a, b);
}

// Función para encontrar la suma de los dígitos de un número
function sumaDigitos(n) {
  if (n == 0) {
    return 0;
  } else {
    return (n % 10) + sumaDigitos(Math.floor(n / 10));
  }
}

// Función para encontrar el producto de los dígitos de un número
function productoDigitos(n) {
  if (n == 0) {
    return 1;
  } else {
    return (n % 10) * productoDigitos(Math.floor(n / 10));
  }
}

// Función para encontrar el número de dígitos de un número
function numDigitos(n) {
  if (n == 0) {
    return 0;
  } else {
    return 1 + numDigitos(Math.floor(n / 10));
  }
}

// Función para encontrar el dígito más significativo de un número
function digitoMasSignificativo(n) {
  while (n >= 10) {
    n = Math.floor(n / 10);
  }
  return n;
}

// Función para encontrar el dígito menos significativo de un número
function digitoMenosSignificativo(n) {
  return n % 10;
}

// Función para encontrar el número inverso de un número
function inverso(n) {
  let inverso = 0;
  while (n > 0) {
    inverso = inverso * 10 + (n % 10);
    n = Math.floor(n / 10);
  }
  return inverso;
}

// Función para encontrar si un número es primo
function esPrimo(n) {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(n); i++) {
    if (n % i == 0) {
      return false;
    }
  }
  return true;
}
```