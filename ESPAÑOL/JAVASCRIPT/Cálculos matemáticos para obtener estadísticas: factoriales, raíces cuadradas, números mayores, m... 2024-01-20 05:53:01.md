```javascript
// Función que recibe un número y devuelve su factorial
function factorial(numero) {
  // Si el número es 0 o 1, devuelve 1
  if (numero === 0 || numero === 1) {
    return 1;
  }

  // Si el número es mayor que 0, devuelve el factorial del número multiplicado por el factorial del número anterior
  else {
    return numero * factorial(numero - 1);
  }
}

// Función que recibe un número y devuelve su raíz cuadrada
function raizCuadrada(numero) {
  // Si el número es 0, devuelve 0
  if (numero === 0) {
    return 0;
  }

  // Si el número es mayor que 0, devuelve la raíz cuadrada del número
  else {
    return Math.sqrt(numero);
  }
}

// Función que recibe dos números y devuelve el número mayor
function numeroMayor(numero1, numero2) {
  // Si el primer número es mayor que el segundo, devuelve el primer número
  if (numero1 > numero2) {
    return numero1;
  }

  // Si el segundo número es mayor que el primero, devuelve el segundo número
  else {
    return numero2;
  }
}

// Función que recibe un array de números y devuelve el número más grande
function numeroMaximo(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve ese elemento
  else if (numeros.length === 1) {
    return numeros[0];
  }

  // Si el array tiene más de un elemento, devuelve el número máximo del array
  else {
    let numeroMax = numeros[0];

    for (let i = 1; i < numeros.length; i++) {
      if (numeros[i] > numeroMax) {
        numeroMax = numeros[i];
      }
    }

    return numeroMax;
  }
}

// Función que recibe un array de números y devuelve el número más pequeño
function numeroMinimo(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve ese elemento
  else if (numeros.length === 1) {
    return numeros[0];
  }

  // Si el array tiene más de un elemento, devuelve el número mínimo del array
  else {
    let numeroMin = numeros[0];

    for (let i = 1; i < numeros.length; i++) {
      if (numeros[i] < numeroMin) {
        numeroMin = numeros[i];
      }
    }

    return numeroMin;
  }
}

// Función que recibe un array de números y devuelve la media de los números
function media(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve ese elemento
  else if (numeros.length === 1) {
    return numeros[0];
  }

  // Si el array tiene más de un elemento, devuelve la media de los números del array
  else {
    let suma = 0;

    for (let i = 0; i < numeros.length; i++) {
      suma += numeros[i];
    }

    return suma / numeros.length;
  }
}

// Función que recibe un array de números y devuelve la desviación estándar de los números
function desviacionEstandar(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve 0
  else if (numeros.length === 1) {
    return 0;
  }

  // Si el array tiene más de un elemento, devuelve la desviación estándar de los números del array
  else {
    let media = media(numeros);
    let sumaCuadrados = 0;

    for (let i = 0; i < numeros.length; i++) {
      sumaCuadrados += Math.pow(numeros[i] - media, 2);
    }

    return Math.sqrt(sumaCuadrados / (numeros.length - 1));
  }
}

// Función que recibe un array de números y devuelve el coeficiente de variación de los números
function coeficienteDeVariacion(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve 0
  else if (numeros.length === 1) {
    return 0;
  }

  // Si el array tiene más de un elemento, devuelve el coeficiente de variación de los números del array
  else {
    let media = media(numeros);
    let desviacionEstandar = desviacionEstandar(numeros);

    return desviacionEstandar / media;
  }
}

// Función que recibe un array de números y devuelve el rango de los números
function rango(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve 0
  else if (numeros.length === 1) {
    return 0;
  }

  // Si el array tiene más de un elemento, devuelve el rango de los números del array
  else {
    let numeroMax = numeroMaximo(numeros);
    let numeroMin = numeroMinimo(numeros);

    return numeroMax - numeroMin;
  }
}

// Función que recibe un array de números y devuelve el cuartil inferior de los números
function cuartilInferior(numeros) {
  // Si el array está vacío, devuelve null
  if (numeros.length === 0) {
    return null;
  }

  // Si el array tiene un solo elemento, devuelve ese elemento
  else if (numeros.length === 1) {
    return numeros[0];
  }

  // Si el array tiene más de un elemento, devuelve el cuartil inferior de los números del array
  else {
    // Ordena el array de menor a mayor
    numeros.sort(function(a, b) {
      return a - b;
    });

    // Calcula el índice del cuartil inferior
    let indiceCuartilInferior = Math.floor(numeros.length / 4);

    // Devuelve el cuartil inferior
    return numeros[indiceCuartilInferior];
  }
}

// Función que recibe un array de números y devuelve el cuartil superior de los números
function cuartilSuperior(numeros) {