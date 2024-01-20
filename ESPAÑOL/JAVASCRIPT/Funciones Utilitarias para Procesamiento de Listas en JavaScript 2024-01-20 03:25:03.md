```javascript
// Definir una función para generar un número aleatorio entre dos valores
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Definir una función para generar una lista de números aleatorios
function generarListaNumerosAleatorios(cantidad, min, max) {
  const listaNumeros = [];
  for (let i = 0; i < cantidad; i++) {
    listaNumeros.push(generarNumeroAleatorio(min, max));
  }
  return listaNumeros;
}

// Definir una función para encontrar el número máximo en una lista
function encontrarMaximo(lista) {
  let maximo = lista[0];
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] > maximo) {
      maximo = lista[i];
    }
  }
  return maximo;
}

// Definir una función para encontrar el número mínimo en una lista
function encontrarMinimo(lista) {
  let minimo = lista[0];
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] < minimo) {
      minimo = lista[i];
    }
  }
  return minimo;
}

// Definir una función para calcular la suma de una lista de números
function calcularSuma(lista) {
  let suma = 0;
  for (let i = 0; i < lista.length; i++) {
    suma += lista[i];
  }
  return suma;
}

// Definir una función para calcular el promedio de una lista de números
function calcularPromedio(lista) {
  const suma = calcularSuma(lista);
  const promedio = suma / lista.length;
  return promedio;
}

// Definir una función para encontrar la mediana de una lista de números
function encontrarMediana(lista) {
  lista.sort((a, b) => a - b);
  const mitad = Math.floor(lista.length / 2);

  if (lista.length % 2 === 0) {
    const mediana = (lista[mitad] + lista[mitad - 1]) / 2;
    return mediana;
  } else {
    const mediana = lista[mitad];
    return mediana;
  }
}

// Definir una función para encontrar la moda de una lista de números
function encontrarModa(lista) {
  const mapa = {};
  lista.forEach((numero) => {
    if (!mapa[numero]) {
      mapa[numero] = 0;
    }
    mapa[numero]++;
  });

  let moda = lista[0];
  let maximo = mapa[lista[0]];

  for (let i = 1; i < lista.length; i++) {
    if (mapa[lista[i]] > maximo) {
      maximo = mapa[lista[i]];
      moda = lista[i];
    }
  }

  return moda;
}

// Definir una función para encontrar los elementos únicos en una lista
function encontrarElementosUnicos(lista) {
  const elementosUnicos = [];
  for (let i = 0; i < lista.length; i++) {
    if (!elementosUnicos.includes(lista[i])) {
      elementosUnicos.push(lista[i]);
    }
  }
  return elementosUnicos;
}

// Definir una función para encontrar los elementos que se repiten en una lista
function encontrarElementosRepetidos(lista) {
  const elementosRepetidos = [];
  const mapa = {};
  for (let i = 0; i < lista.length; i++) {
    if (!mapa[lista[i]]) {
      mapa[lista[i]] = 0;
    }
    mapa[lista[i]]++;

    if (mapa[lista[i]] > 1) {
      elementosRepetidos.push(lista[i]);
    }
  }
  return elementosRepetidos;
}

// Definir una función para encontrar el elemento más frecuente en una lista
function encontrarElementoMasFrecuente(lista) {
  const mapa = {};
  lista.forEach((numero) => {
    if (!mapa[numero]) {
      mapa[numero] = 0;
    }
    mapa[numero]++;
  });

  let elementoMasFrecuente = lista[0];
  let maximo = mapa[lista[0]];

  for (let i = 1; i < lista.length; i++) {
    if (mapa[lista[i]] > maximo) {
      maximo = mapa[lista[i]];
      elementoMasFrecuente = lista[i];
    }
  }

  return elementoMasFrecuente;
}

// Definir una función para encontrar los elementos que faltan en una lista
function encontrarElementosFaltantes(lista) {
  const elementosFaltantes = [];
  const min = encontrarMinimo(lista);
  const max = encontrarMaximo(lista);

  for (let i = min; i <= max; i++) {
    if (!lista.includes(i)) {
      elementosFaltantes.push(i);
    }
  }

  return elementosFaltantes;
}

// Definir una función para encontrar los elementos que están en una lista pero no en otra
function encontrarElementosDiferentes(lista1, lista2) {
  const elementosDiferentes = [];

  lista1.forEach((elemento) => {
    if (!lista2.includes(elemento)) {
      elementosDiferentes.push(elemento);
    }
  });

  lista2.forEach((elemento) => {
    if (!lista1.includes(elemento)) {
      elementosDiferentes.push(elemento);
    }
  });

  return elementosDiferentes;
}

// Definir una función para encontrar la unión de dos listas
function encontrarUnion(lista1, lista2) {
  const union = [...lista1, ...lista2];
  return union;
}

// Definir una función para encontrar la intersección de dos listas
function encontrarInterseccion(lista1, lista2) {
  const interseccion = [];
  lista1.forEach((elemento) => {
    if (lista2.includes(elemento)) {
      interseccion.push(elemento);
    }
  });
  return interseccion;
}

// Definir una función para encontrar la diferencia de dos listas
function encontrarDiferencia(lista1, lista2) {
  const diferencia = [];
  lista1.forEach((elemento) => {
    if (!lista2.includes(elemento)) {
      diferencia.push(elemento);
    }
  });
  return diferencia;
}

// Definir una función para encontrar el producto escalar de dos listas
function encontrarProductoEscalar(lista1, lista2) {
  const productoEscalar = 0;
  for (let i = 0; i < lista1.length; i++) {
    productoEscalar += lista1[i] * lista2[i];
  }
  return productoEscalar;
}

// Definir una función para encontrar el ángulo entre dos listas
function encontrarAngulo(lista1, lista2) {
  const productoEscalar = encontrarProductoEscalar(lista1, lista2);
  const magnitud1 = encontrarMagnitud(lista1);
  const magnitud2 = encontrarMagnitud(lista2);
  const angulo = Math.acos(productoEscalar / (magnitud1 * magnitud2));
  return angulo;
}

// Definir una función para encontrar la distancia entre dos puntos
function encontrarDistancia(punto1, punto2) {
  const distancia = Math.sqrt(
    Math.pow(punto2