```javascript
// Definimos una función llamada 'fibonacci' que calcula la secuencia de Fibonacci hasta un número determinado de términos.
function fibonacci(numero) {
  // Si el número es menor o igual que 1, devolvemos el propio número.
  if (numero <= 1) {
    return numero;
  }
  // Si el número es mayor que 1, devolvemos la suma de los dos términos anteriores de la secuencia.
  else {
    return fibonacci(numero - 1) + fibonacci(numero - 2);
  }
}

// Definimos una función llamada 'factorial' que calcula el factorial de un número.
function factorial(numero) {
  // Si el número es menor o igual que 1, devolvemos el propio número.
  if (numero <= 1) {
    return 1;
  }
  // Si el número es mayor que 1, devolvemos el número multiplicado por el factorial del número anterior.
  else {
    return numero * factorial(numero - 1);
  }
}

// Definimos una función llamada 'primo' que comprueba si un número es primo o no.
function primo(numero) {
  // Si el número es menor o igual que 1, devolvemos false porque no es primo.
  if (numero <= 1) {
    return false;
  }
  // Si el número es mayor que 1, comprobamos si es divisible por alguno de los números entre 2 y la raíz cuadrada del número.
  else {
    for (var i = 2; i <= Math.sqrt(numero); i++) {
      if (numero % i === 0) {
        return false;
      }
    }
    // Si el número no es divisible por ninguno de los números entre 2 y su raíz cuadrada, devolvemos true porque es primo.
    return true;
  }
}

// Definimos una función llamada 'mayorPrimo' que devuelve el mayor número primo menor o igual a un número determinado.
function mayorPrimo(numero) {
  // Si el número es menor o igual que 1, devolvemos 0 porque no hay números primos menores o iguales que 1.
  if (numero <= 1) {
    return 0;
  }
  // Si el número es mayor que 1, comprobamos si es primo y, si lo es, devolvemos el propio número.
  else {
    if (primo(numero)) {
      return numero;
    }
    // Si el número no es primo, devolvemos el mayor número primo menor o igual al número anterior.
    else {
      return mayorPrimo(numero - 1);
    }
  }
}

// Definimos una función llamada 'menorPrimo' que devuelve el menor número primo mayor o igual a un número determinado.
function menorPrimo(numero) {
  // Si el número es menor o igual que 1, devolvemos 2 porque es el primer número primo.
  if (numero <= 1) {
    return 2;
  }
  // Si el número es mayor que 1, comprobamos si es primo y, si lo es, devolvemos el propio número.
  else {
    if (primo(numero)) {
      return numero;
    }
    // Si el número no es primo, devolvemos el menor número primo mayor o igual al número siguiente.
    else {
      return menorPrimo(numero + 1);
    }
  }
}

// Definimos una función llamada 'sumaDigitos' que devuelve la suma de los dígitos de un número.
function sumaDigitos(numero) {
  // Si el número es menor que 0, devolvemos 0 porque no tiene dígitos.
  if (numero < 0) {
    return 0;
  }
  // Si el número es mayor o igual que 0, devolvemos la suma de los dígitos del número.
  else {
    return (numero % 10) + sumaDigitos(Math.floor(numero / 10));
  }
}

// Definimos una función llamada 'productoDigitos' que devuelve el producto de los dígitos de un número.
function productoDigitos