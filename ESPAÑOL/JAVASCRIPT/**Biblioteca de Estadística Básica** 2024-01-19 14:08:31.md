```javascript
// Crear una función para generar un número aleatorio entre un rango especificado
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Crear una función para mezclar un arreglo de elementos
function mezclarArreglo(arreglo) {
  for (let i = arreglo.length - 1; i > 0; i--) {
    let j = generarNumeroAleatorio(0, i);
    let temp = arreglo[i];
    arreglo[i] = arreglo[j];
    arreglo[j] = temp;
  }
  return arreglo;
}

// Crear una función para encontrar el elemento más pequeño en un arreglo
function encontrarElementoMasPequeno(arreglo) {
  let elementoMasPequeno = arreglo[0];
  for (let i = 1; i < arreglo.length; i++) {
    if (arreglo[i] < elementoMasPequeno) {
      elementoMasPequeno = arreglo[i];
    }
  }
  return elementoMasPequeno;
}

// Crear una función para encontrar el elemento más grande en un arreglo
function encontrarElementoMasGrande(arreglo) {
  let elementoMasGrande = arreglo[0];
  for (let i = 1; i < arreglo.length; i++) {
    if (arreglo[i] > elementoMasGrande) {
      elementoMasGrande = arreglo[i];
    }
  }
  return elementoMasGrande;
}

// Crear una función para encontrar el promedio de los elementos en un arreglo
function encontrarPromedio(arreglo) {
  let suma = 0;
  for (let i = 0; i < arreglo.length; i++) {
    suma += arreglo[i];
  }
  return suma / arreglo.length;
}

// Crear una función para encontrar la mediana de los elementos en un arreglo
function encontrarMediana(arreglo) {
  // Ordenar el arreglo en orden ascendente
  arreglo.sort((a, b) => a - b);
  
  // Si el arreglo tiene un número par de elementos, la mediana es el promedio de los dos elementos centrales
  if (arreglo.length % 2 === 0) {
    return (arreglo[arreglo.length / 2 - 1] + arreglo[arreglo.length / 2]) / 2;
  }
  
  // Si el arreglo tiene un número impar de elementos, la mediana es el elemento central
  else {
    return arreglo[Math.floor(arreglo.length / 2)];
  }
}

// Crear una función para encontrar el rango de los elementos en un arreglo
function encontrarRango(arreglo) {
  let elementoMasGrande = encontrarElementoMasGrande(arreglo);
  let elementoMasPequeno = encontrarElementoMasPequeno(arreglo);
  return elementoMasGrande - elementoMasPequeno;
}

// Crear una función para encontrar la desviación estándar de los elementos en un arreglo
function encontrarDesviacionEstandar(arreglo) {
  let promedio = encontrarPromedio(arreglo);
  let desviacionEstandar = 0;
  
  for (let i = 0; i < arreglo.length; i++) {
    desviacionEstandar += Math.pow(arreglo[i] - promedio, 2);
  }
  
  desviacionEstandar /= arreglo.length;
  desviacionEstandar = Math.sqrt(desviacionEstandar);
  
  return desviacionEstandar;
}

// Probar las funciones con un arreglo de números
let arreglo = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

console.log("Número aleatorio entre 1 y 10:", generarNumeroAleatorio(1, 10));
console.log("Arreglo mezclado:", mezclarArreglo(arreglo));
console.log("Elemento más pequeño:", encontrarElementoMasPequeno(arreglo));
console.log("Elemento más grande:", encontrarElementoMasGrande(arreglo));
console.log("Promedio:", encontrarPromedio(arreglo));
console.log("Mediana:", encontrarMediana(arreglo));
console.log("Rango:", encontrarRango(arreglo));
console.log("Desviación estándar:", encontrarDesviacionEstandar(arreglo));
```

Explicación del código:

1. **Función `generarNumeroAleatorio()`:**  Esta función genera un número aleatorio entre dos valores especificados.
2. **Función `mezclarArreglo()`:** Esta función mezcla los elementos de un arreglo.
3. **Función `encontrarElementoMasPequeno()`:** Esta función encuentra el elemento más pequeño en un arreglo.
4. **Función `encontrarElementoMasGrande()`:** Esta función encuentra el elemento más grande en un arreglo.
5. **Función `encontrarPromedio()`:** Esta función encuentra el promedio de los elementos en un arreglo.
6. **Función `encontrarMediana()`:** Esta función encuentra la mediana de los elementos en un arreglo.
7. **Función `encontrarRango()`:** Esta función encuentra el rango de los elementos en un arreglo.
8. **Función `encontrarDesviacionEstandar()`:** Esta función encuentra la desviación estándar de los elementos en un arreglo.

El código se prueba con un arreglo de números y los resultados se imprimen en la consola.