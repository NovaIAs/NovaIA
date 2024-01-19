```javascript
// Función recursiva para encontrar el factorial de un número
function factorial(numero) {
  if (numero === 0) {
    return 1;
  } else {
    return numero * factorial(numero - 1);
  }
}

// Función para generar números aleatorios entre dos valores dados
function generarNumeroAleatorio(minimo, maximo) {
  return Math.floor(Math.random() * (maximo - minimo + 1)) + minimo;
}

// Función para buscar el elemento máximo de una lista
function encontrarMaximo(lista) {
  if (lista.length === 0) {
    return null;
  }

  let maximo = lista[0];
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] > maximo) {
      maximo = lista[i];
    }
  }

  return maximo;
}

// Función para buscar el elemento mínimo de una lista
function encontrarMinimo(lista) {
  if (lista.length === 0) {
    return null;
  }

  let minimo = lista[0];
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] < minimo) {
      minimo = lista[i];
    }
  }

  return minimo;
}

// Función para ordenar una lista de números en orden ascendente
function ordenarAscendente(lista) {
  for (let i = 0; i < lista.length - 1; i++) {
    for (let j = i + 1; j < lista.length; j++) {
      if (lista[i] > lista[j]) {
        let temp = lista[i];
        lista[i] = lista[j];
        lista[j] = temp;
      }
    }
  }

  return lista;
}

// Función para ordenar una lista de números en orden descendente
function ordenarDescendente(lista) {
  for (let i = 0; i < lista.length - 1; i++) {
    for (let j = i + 1; j < lista.length; j++) {
      if (lista[i] < lista[j]) {
        let temp = lista[i];
        lista[i] = lista[j];
        lista[j] = temp;
      }
    }
  }

  return lista;
}

// Función para calcular la media de una lista de números
function calcularMedia(lista) {
  if (lista.length === 0) {
    return null;
  }

  let suma = 0;
  for (let i = 0; i < lista.length; i++) {
    suma += lista[i];
  }

  return suma / lista.length;
}

// Función para calcular la mediana de una lista de números
function calcularMediana(lista) {
  if (lista.length === 0) {
    return null;
  }

  lista = ordenarAscendente(lista);

  let mitad = Math.floor(lista.length / 2);
  if (lista.length % 2 === 0) {
    return (lista[mitad] + lista[mitad - 1]) / 2;
  } else {
    return lista[mitad];
  }
}

// Función para calcular la moda de una lista de números
function calcularModa(lista) {
  if (lista.length === 0) {
    return null;
  }

  let moda = lista[0];
  let frecuencia = 1;

  for (let i = 1; i < lista.length; i++) {
    let count = 0;
    for (let j = 0; j < i; j++) {
      if (lista[i] === lista[j]) {
        count++;
      }
    }

    if (count > frecuencia) {
      moda = lista[i];
      frecuencia = count;
    }
  }

  return moda;
}

// Función para crear un objeto a partir de una lista de pares clave-valor
function crearObjeto(claves, valores) {
  if (claves.length !== valores.length) {
    throw new Error("Las listas de claves y valores deben tener la misma longitud.");
  }

  let objeto = {};
  for (let i = 0; i < claves.length; i++) {
    objeto[claves[i]] = valores[i];
  }

  return objeto;
}

// Función para añadir un nuevo elemen