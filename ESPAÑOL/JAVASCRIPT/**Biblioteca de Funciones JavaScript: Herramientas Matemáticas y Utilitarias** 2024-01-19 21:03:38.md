```javascript
// Definimos la función principal del programa
function programaPrincipal() {
  // Creamos una matriz con los números del 1 al 10
  var numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Creamos un objeto con los nombres de las frutas
  var frutas = {
    manzana: "manzana",
    naranja: "naranja",
    pera: "pera",
    uva: "uva",
    platano: "plátano"
  };

  // Creamos una función para sumar dos números
  function sumar(a, b) {
    return a + b;
  }

  // Creamos una función para restar dos números
  function restar(a, b) {
    return a - b;
  }

  // Creamos una función para multiplicar dos números
  function multiplicar(a, b) {
    return a * b;
  }

  // Creamos una función para dividir dos números
  function dividir(a, b) {
    return a / b;
  }

  // Creamos una función para calcular el factorial de un número
  function factorial(n) {
    if (n === 0) {
      return 1;
    }
    return n * factorial(n - 1);
  }

  // Creamos una función para calcular el máximo común divisor de dos números
  function mcd(a, b) {
    if (b === 0) {
      return a;
    }
    return mcd(b, a % b);
  }

  // Creamos una función para calcular el mínimo común múltiplo de dos números
  function mcm(a, b) {
    return (a * b) / mcd(a, b);
  }

  // Creamos una función para comprobar si un número es primo
  function esPrimo(n) {
    if (n <= 1) {
      return false;
    }
    for (var i = 2; i <= Math.sqrt(n); i++) {
      if (n % i === 0) {
        return false;
      }
    }
    return true;
  }

  // Creamos una función para generar una lista de números primos hasta un número dado
  function generarPrimos(n) {
    var primos = [];
    for (var i = 2; i <= n; i++) {
      if (esPrimo(i)) {
        primos.push(i);
      }
    }
    return primos;
  }

  // Creamos una función para ordenar una matriz de números de menor a mayor
  function ordenar(numeros) {
    return numeros.sort(function(a, b) {
      return a - b;
    });
  }

  // Creamos una función para buscar un elemento en una matriz
  function buscar(elemento, matriz) {
    return matriz.indexOf(elemento);
  }

  // Creamos una función para eliminar un elemento de una matriz
  function eliminar(elemento, matriz) {
    var indice = buscar(elemento, matriz);
    if (indice !== -1) {
      matriz.splice(indice, 1);
    }
  }

  // Creamos una función para añadir un elemento a una matriz
  function añadir(elemento, matriz) {
    matriz.push(elemento);
  }

  // Creamos una función para crear un objeto con las propiedades de un objeto
  function crearObjeto(objeto) {
    var nuevoObjeto = {};
    for (var propiedad in objeto) {
      nuevoObjeto[propiedad] = objeto[propiedad];
    }
    return nuevoObjeto;
  }

  // Creamos una función para copiar una matriz
  function copiarMatriz(matriz) {
    var nuevaMatriz = [];
    for (var i = 0; i < matriz.length; i++) {
      nuevaMatriz.push(matriz[i]);
    }
    return nuevaMatriz;
  }

  // Creamos una función para mezclar una matriz
  function mezclarMatriz(matriz) {
    for (var i = 0; i < matriz.length; i++) {
      var indiceAleatorio = Math.floor(Math.random() * matriz.length);
      var elementoTemporal = matriz[i];
      matriz[i] = matriz[indiceAleatorio];
      matriz[indiceAleatorio] = elementoTemporal;
    }
    return matriz;
  }

  // Creamos una función para generar un número aleatorio entre dos números