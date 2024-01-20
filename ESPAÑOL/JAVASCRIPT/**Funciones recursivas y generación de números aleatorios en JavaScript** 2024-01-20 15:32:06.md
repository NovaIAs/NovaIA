```javascript
// Definir una función recursiva para calcular el factorial de un número.

function factorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Imprimir los factoriales de los primeros 10 números enteros positivos.

for (let i = 0; i <= 10; i++) {
  console.log(`El factorial de ${i} es ${factorial(i)}.`);
}


// Definir una función para convertir un número decimal a binario.

function decimalToBinary(n) {
  if (n === 0) {
    return "0";
  } else {
    return decimalToBinary(Math.floor(n / 2)) + (n % 2).toString();
  }
}

// Imprimir las representaciones binarias de los primeros 10 números enteros positivos.

for (let i = 0; i <= 10; i++) {
  console.log(`La representación binaria de ${i} es ${decimalToBinary(i)}.`);
}


// Definir una función para generar un número aleatorio entre 0 y 1 (excluyendo 1).

function randomFloat() {
  return Math.random();
}

// Imprimir 10 números aleatorios entre 0 y 1 (excluyendo 1).

for (let i = 0; i < 10; i++) {
  console.log(`Número aleatorio ${i + 1}: ${randomFloat()}`);
}


// Definir una función para calcular el máximo común divisor (MCD) de dos números.

function gcd(a, b) {
  if (b === 0) {
    return a;
  } else {
    return gcd(b, a % b);
  }
}

// Imprimir el MCD de los primeros 10 pares de números enteros positivos.

for (let i = 1; i <= 10; i++) {
  for (let j = i + 1; j <= 10; j++) {
    console.log(`El MCD de ${i} y ${j} es ${gcd(i, j)}.`);
  }
}


// Definir una función para generar una cadena aleatoria de caracteres alfanuméricos.

function randomString(length) {
  const characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";

  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * characters.length));
  }

  return result;
}

// Imprimir 10 cadenas aleatorias de 10 caracteres cada una.

for (let i = 0; i < 10; i++) {
  console.log(`Cadena aleatoria ${i + 1}: ${randomString(10)}.`);
}
```

Explicación:

* La primera función, `factorial`, calcula el factorial de un número usando recursividad.
* La segunda función, `decimalToBinary`, convierte un número decimal a binario usando recursividad.
* La tercera función, `randomFloat`, genera un número aleatorio entre 0 y 1 (excluyendo 1).
* La cuarta función, `gcd`, calcula el máximo común divisor (MCD) de dos números usando recursividad.
* La quinta función, `randomString`, genera una cadena aleatoria de caracteres alfanuméricos.

El código imprime los resultados de estas funciones en la consola.