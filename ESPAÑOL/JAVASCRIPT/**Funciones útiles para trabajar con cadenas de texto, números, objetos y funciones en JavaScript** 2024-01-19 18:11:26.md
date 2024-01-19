```javascript
// Crear una función que reciba una cadena de texto y devuelva un array con las palabras de la cadena.

const separarPalabras = (texto) => {
  // Comprobar si el texto es una cadena de texto.
  if (typeof texto !== "string") {
    throw new Error("El texto debe ser una cadena de texto.");
  }

  // Eliminar los espacios en blanco al principio y al final del texto.
  texto = texto.trim();

  // Dividir el texto en palabras usando el espacio en blanco como separador.
  const palabras = texto.split(/\s+/);

  // Devolver el array con las palabras.
  return palabras;
};

// Crear una función que reciba un array de números y devuelva la suma de los números del array.

const sumaNumeros = (numeros) => {
  // Comprobar si el array es un array de números.
  if (!Array.isArray(numeros) || numeros.some((numero) => typeof numero !== "number")) {
    throw new Error("El array debe ser un array de números.");
  }

  // Sumar los números del array utilizando el método reduce().
  const suma = numeros.reduce((acumulador, numero) => acumulador + numero, 0);

  // Devolver la suma de los números.
  return suma;
};

// Crear una función que reciba un objeto y devuelva un array con las claves del objeto.

const obtenerClaves = (objeto) => {
  // Comprobar si el objeto es un objeto.
  if (typeof objeto !== "object" || Array.isArray(objeto)) {
    throw new Error("El objeto debe ser un objeto.");
  }

  // Obtener las claves del objeto utilizando el método Object.keys().
  const claves = Object.keys(objeto);

  // Devolver el array con las claves.
  return claves;
};

// Crear una función que reciba una función y devuelva una nueva función que llame a la función recibida con los argumentos invertidos.

const invertirArgumentos = (funcion) => {
  // Comprobar si la función es una función.
  if (typeof funcion !== "function") {
    throw new Error("La función debe ser una función.");
  }

  // Crear una nueva función que llame a la función recibida con los argumentos invertidos.
  const nuevaFuncion = (...argumentos) => funcion(...argumentos.reverse());

  // Devolver la nueva función.
  return nuevaFuncion;
};
```