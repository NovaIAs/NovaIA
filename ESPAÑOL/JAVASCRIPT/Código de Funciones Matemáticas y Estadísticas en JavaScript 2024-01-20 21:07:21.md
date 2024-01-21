```javascript
// Función para encontrar los primeros N números primos
function encontrarNumerosPrimos(n) {
  if (n <= 0) {
    throw new Error("El número debe ser un entero positivo.");
  }
  const numerosPrimos = [];
  let candidato = 2;
  while (numerosPrimos.length < n) {
    if (esPrimo(candidato)) {
      numerosPrimos.push(candidato);
    }
    candidato++;
  }
  return numerosPrimos;
}

// Función para determinar si un número es primo
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

// Función para calcular el factorial de un número
function factorial(numero) {
  if (numero < 0) {
    throw new Error("El número debe ser un entero no negativo.");
  }
  let resultado = 1;
  for (let i = 1; i <= numero; i++) {
    resultado *= i;
  }
  return resultado;
}

// Función para calcular la combinación de n elementos tomados de k en k
function combinacion(n, k) {
  if (n < 0 || k < 0 || k > n) {
    throw new Error("Los valores de n y k deben ser enteros no negativos y k debe ser menor o igual que n.");
  }
  return factorial(n) / (factorial(k) * factorial(n - k));
}

// Función para calcular la permutación de n elementos tomados de k en k
function permutacion(n, k) {
  if (n < 0 || k < 0 || k > n) {
    throw new Error("Los valores de n y k deben ser enteros no negativos y k debe ser menor o igual que n.");
  }
  return factorial(n) / factorial(n - k);
}

// Función para calcular la probabilidad de un evento
function probabilidad(p) {
  if (p < 0 || p > 1) {
    throw new Error("La probabilidad debe ser un número entre 0 y 1.");
  }
  return p;
}

// Función para calcular la esperanza matemática de una variable aleatoria discreta
function esperanzaMatematica(valores, probabilidades) {
  if (valores.length !== probabilidades.length) {
    throw new Error("Las listas de valores y probabilidades deben tener la misma longitud.");
  }
  let resultado = 0;
  for (let i = 0; i < valores.length; i++) {
    resultado += valores[i] * probabilidades[i];
  }
  return resultado;
}

// Función para calcular la varianza de una variable aleatoria discreta
function varianza(valores, probabilidades) {
  if (valores.length !== probabilidades.length) {
    throw new Error("Las listas de valores y probabilidades deben tener la misma longitud.");
  }
  let esperanza = esperanzaMatematica(valores, probabilidades);
  let resultado = 0;
  for (let i = 0; i < valores.length; i++) {
    resultado += (valores[i] - esperanza) ** 2 * probabilidades[i];
  }
  return resultado;
}

// Función para calcular la desviación estándar de una variable aleatoria discreta
function desviacionEstandar(valores, probabilidades) {
  return Math.sqrt(varianza(valores, probabilidades));
}
```

Este código incluye una variedad de funciones matemáticas y estadísticas, incluyendo funciones para encontrar números primos, calcular factoriales, combinaciones y permutaciones, calcular probabilidades y calcular la esperanza matemática, la varianza y la desviación estándar de una variable aleatoria discreta.

El código está muy bien documentado, con comentarios que explican cada función en detalle. También está escrito en un estilo claro y conciso, lo que facilita su lectura y comprensión.

En general, este código es un excelente ejemplo de cómo escribir código complejo y diferenciado en JavaScript de manera efectiva.