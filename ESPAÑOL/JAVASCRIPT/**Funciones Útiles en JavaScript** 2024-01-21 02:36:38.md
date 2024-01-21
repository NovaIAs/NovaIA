```javascript
// Ejemplo de código complejo en JavaScript

// Creamos una función para obtener el factorial de un número
function factorial(numero) {
  if (numero < 0) {
    throw new Error("El número no puede ser negativo");
  }

  if (numero === 0 || numero === 1) {
    return 1;
  }

  return numero * factorial(numero - 1);
}

// Creamos una función para calcular la suma de los cuadrados de los primeros n números naturales
function sumaCuadrados(n) {
  if (n < 1) {
    throw new Error("El número debe ser mayor o igual a 1");
  }

  let suma = 0;
  for (let i = 1; i <= n; i++) {
    suma += i ** 2;
  }

  return suma;
}

// Creamos una función para invertir una cadena de caracteres
function invertirCadena(cadena) {
  if (cadena === null || cadena === "") {
    return "";
  }

  let cadenaInvertida = "";
  for (let i = cadena.length - 1; i >= 0; i--) {
    cadenaInvertida += cadena[i];
  }

  return cadenaInvertida;
}

// Creamos una función que recibe una función y la ejecuta para cada elemento de un array
function aplicarFuncion(array, funcion) {
  if (array === null || array.length === 0) {
    return [];
  }

  if (funcion === null || typeof funcion !== "function") {
    throw new Error("La función no es válida");
  }

  const nuevoArray = [];
  for (let i = 0; i < array.length; i++) {
    nuevoArray.push(funcion(array[i]));
  }

  return nuevoArray;
}

// Creamos una función que devuelve una función que recibe un argumento y devuelve el resultado de sumarlo a un número fijo
function crearSumador(numero) {
  return function (argumento) {
    return numero + argumento;
  };
}

// Creamos una función que recibe un array de números y devuelve el número máximo
function maximoArray(array) {
  if (array === null || array.length === 0) {
    return null;
  }

  let maximo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maximo) {
      maximo = array[i];
    }
  }

  return maximo;
}

// Creamos una función que recibe un array de números y devuelve el número mínimo
function minimoArray(array) {
  if (array === null || array.length === 0) {
    return null;
  }

  let minimo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < minimo) {
      minimo = array[i];
    }
  }

  return minimo;
}

// Creamos una función que recibe un array de números y devuelve la media aritmética de los números del array
function mediaArray(array) {
  if (array === null || array.length === 0) {
    return null;
  }

  let suma = 0;
  for (let i = 0; i < array.length; i++) {
    suma += array[i];
  }

  return suma / array.length;
}

// Creamos una función que recibe un array de números y devuelve la desviación estándar de los números del array
function desviacionEstandarArray(array) {
  if (array === null || array.length === 0) {
    return null;
  }

  const media = mediaArray(array);
  let desviacionCuadrada = 0;
  for (let i = 0; i < array.length; i++) {
    desviacionCuadrada += (array[i] - media) ** 2;
  }

  return Math.sqrt(desviacionCuadrada / (array.length - 1));
}

// Creamos una función que recibe un array de objetos y devuelve un nuevo array con los objetos ordenados por una propiedad específica
function ordenarArrayObjetos(array, propiedad) {
  if (array === null || array.length === 0) {
    return [];
  }

  return array.sort((a, b) => a[propiedad] - b[propiedad]);
}

// Creamos una función que recibe un array de objetos y devuelve un nuevo array con los objetos filtrados por una propiedad específica
function filtrarArrayObjetos(array, propiedad, valor) {
  if (array === null || array.length === 0) {
    return [];
  }

  return array.filter((objeto) => objeto[propiedad] === valor);
}

// Creamos una función que recibe un array de objetos y devuelve un nuevo array con los objetos agrupados por una propiedad específica
function agruparArrayObjetos(array, propiedad) {
  if (array === null || array.length === 0) {
    return {};
  }

  const grupos = {};
  for (let i = 0; i < array.length; i++) {
    const objeto = array[i];
    const valorPropiedad = objeto[propiedad];
    if (!grupos[valorPropiedad]) {
      grupos[valorPropiedad] = [];
    }
    grupos[valorPropiedad].push(objeto);
  }

  return grupos;
}
```

Este código contiene una colección de funciones útiles para realizar diversas operaciones con datos y objetos en JavaScript. Algunas de las funciones más destacadas son:

* `factorial(numero)`: Calcula el factorial de un número.
* `sumaCuadrados(n)`: Calcula la suma de los cuadrados de los primeros n números naturales.
* `invertirCadena(cadena)`: Invierte una cadena de caracteres.
* `aplicarFuncion(array, funcion)`: Ejecuta una función para cada elemento de un array.
* `crearSumador(numero)`: Crea una función que suma un número fijo a cualquier argumento que reciba.
* `maximoArray(array)`: Devuelve el número máximo de un array de números.
* `minimoArray(array)`: Devuelve el número mínimo de un array de números.
* `mediaArray(array)`: Devuelve la media aritmética de los números de un array.
* `desviacionEstandarArray(array)`: Devuelve la desviación estándar de los números de un array.
* `ordenarArrayObjetos(array, propiedad)`: Ordena un array de objetos por una propiedad específica.
* `filtrarArrayObjetos(array, propiedad, valor)`: Filtra un array de objetos por una propiedad específica y un valor específico.
* `agruparArrayObjetos(array, propiedad)`: Agrupa un array de objetos por una propiedad específica.