```javascript
// Definir una función que recibe un número y devuelve su factorial

function factorial(numero) {
  // Si el número es menor o igual a 1, devolver 1
  if (numero <= 1) {
    return 1;
  }

  // Si el número es mayor que 1, calcular el factorial recursivamente
  else {
    return numero * factorial(numero - 1);
  }
}

// Pedir al usuario que introduzca un número
const numero = parseInt(prompt("Introduce un número: "));

// Calcular el factorial del número introducido
const factorialResultado = factorial(numero);

// Mostrar el factorial del número introducido
alert(`El factorial de ${numero} es ${factorialResultado}`);

// Definir una función que recibe un número y devuelve si es primo

function esPrimo(numero) {
  // Si el número es menor o igual a 1, devolver falso
  if (numero <= 1) {
    return false;
  }

  // Si el número es 2, devolver verdadero
  else if (numero === 2) {
    return true;
  }

  // Si el número es par, devolver falso
  else if (numero % 2 === 0) {
    return false;
  }

  // Si el número no es par, comprobar si es divisible por algún número impar menor que su raíz cuadrada
  else {
    for (let i = 3; i <= Math.sqrt(numero); i += 2) {
      if (numero % i === 0) {
        return false;
      }
    }

    // Si el número no es divisible por ningún número impar menor que su raíz cuadrada, devolver verdadero
    return true;
  }
}

// Pedir al usuario que introduzca un número
const numero2 = parseInt(prompt("Introduce un número: "));

// Comprobar si el número introducido es primo
const esPrimoResultado = esPrimo(numero2);

// Mostrar si el número introducido es primo o no
alert(`El número ${numero2} es ${esPrimoResultado ? "primo" : "no primo"}`);

// Definir una función que recibe una palabra y devuelve su anagrama

function anagrama(palabra) {
  // Convertir la palabra a minúsculas y dividirla en un array de letras
  const letras = palabra.toLowerCase().split("");

  // Barajar el array de letras
  for (let i = 0; i < letras.length; i++) {
    const j = Math.floor(Math.random() * letras.length);
    const temp = letras[i];
    letras[i] = letras[j];
    letras[j] = temp;
  }

  // Unir el array de letras de nuevo en una palabra
  return letras.join("");
}

// Pedir al usuario que introduzca una palabra
const palabra = prompt("Introduce una palabra: ");

// Generar un anagrama de la palabra introducida
const anagramaResultado = anagrama(palabra);

// Mostrar el anagrama de la palabra introducida
alert(`El anagrama de ${palabra} es ${anagramaResultado}`);

// Fin del código
```

Explicación del código:

* La función `factorial` calcula el factorial de un número dado. El factorial de un número es el producto de todos los números enteros positivos menores o iguales que ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.
* La función `esPrimo` comprueba si un número dado es primo. Un número primo es un número entero positivo mayor que 1 que sólo es divisible por 1 y por sí mismo. Por ejemplo, 13 es un número primo, mientras que 12 no lo es.
* La función `anagrama` genera un anagrama de una palabra dada. Un anagrama es una palabra formada por las mismas letras que otra palabra, pero en un orden diferente. Por ejemplo, la palabra "gato" tiene el anagrama "gota".