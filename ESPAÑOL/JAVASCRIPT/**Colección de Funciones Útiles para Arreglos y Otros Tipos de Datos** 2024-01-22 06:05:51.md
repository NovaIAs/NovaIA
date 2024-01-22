```javascript
// Crea una función que reciba un número y devuelva su factorial.
function factorial(n) {
  if (n < 0) {
    throw new Error("El número no puede ser negativo");
  } else if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Crea una función que reciba una cadena y devuelva un objeto con la frecuencia de cada carácter.
function frecuenciaCaracteres(cadena) {
  const caracteres = {};
  for (let i = 0; i < cadena.length; i++) {
    const caracter = cadena[i];
    if (caracteres[caracter]) {
      caracteres[caracter]++;
    } else {
      caracteres[caracter] = 1;
    }
  }
  return caracteres;
}

// Crea una función que reciba un array y devuelva el elemento más grande.
function elementoMasGrande(array) {
  let elementoMasGrande = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > elementoMasGrande) {
      elementoMasGrande = array[i];
    }
  }
  return elementoMasGrande;
}

// Crea una función que reciba un array y devuelva el elemento más pequeño.
function elementoMasPequeño(array) {
  let elementoMasPequeño = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < elementoMasPequeño) {
      elementoMasPequeño = array[i];
    }
  }
  return elementoMasPequeño;
}

// Crea una función que reciba un array y devuelva la media de sus elementos.
function media(array) {
  let suma = 0;
  for (let i = 0; i < array.length; i++) {
    suma += array[i];
  }
  return suma / array.length;
}

// Crea una función que reciba un array y devuelva la mediana de sus elementos.
function mediana(array) {
  array.sort((a, b) => a - b);
  if (array.length % 2 === 0) {
    return (array[array.length / 2 - 1] + array[array.length / 2]) / 2;
  } else {
    return array[(array.length - 1) / 2];
  }
}

// Crea una función que reciba un array y devuelva la moda de sus elementos.
function moda(array) {
  const frecuenciaCaracteres = {};
  let moda;
  let frecuenciaModa = 0;
  for (let i = 0; i < array.length; i++) {
    const elemento = array[i];
    if (frecuenciaCaracteres[elemento]) {
      frecuenciaCaracteres[elemento]++;
    } else {
      frecuenciaCaracteres[elemento] = 1;
    }
    if (frecuenciaCaracteres[elemento] > frecuenciaModa) {
      frecuenciaModa = frecuenciaCaracteres[elemento];
      moda = elemento;
    }
  }
  return moda;
}

// Crea una función que reciba dos arrays y devuelva un nuevo array con los elementos que están en ambos arrays.
function interseccion(array1, array2) {
  const interseccion = [];
  for (let i = 0; i < array1.length; i++) {
    if (array2.includes(array1[i])) {
      interseccion.push(array1[i]);
    }
  }
  return interseccion;
}

// Crea una función que reciba dos arrays y devuelva un nuevo array con los elementos que están en cualquiera de los dos arrays.
function union(array1, array2) {
  const union = [...array1, ...array2];
  return union;
}

// Crea una función que reciba un array y devuelva un nuevo array con los elementos ordenados de menor a mayor.
function ordenar(array) {
  array.sort((a, b) => a - b);
  return array;
}

// Crea una función que reciba un array y devuelva un nuevo array con los elementos ordenados de mayor a menor.
function ordenarDescendente(array) {
  array.sort((a, b) => b - a);
  return array;
}

// Crea una función que reciba un array y devuelva un nuevo array con los elementos únicos.
function eliminarRepetidos(array) {
  const elementosUnicos = new Set(array);
  return Array.from(elementosUnicos);
}

// Crea una función que reciba un array y devuelva un nuevo array con los elementos invertidos.
function invertir(array) {
  const arrayInvertido = [];
  for (let i = array.length - 1; i >= 0; i--) {
    arrayInvertido.push(array[i]);
  }
  return arrayInvertido;
}
```

Este código contiene una colección de funciones útiles para trabajar con arrays y otros tipos de datos en JavaScript. Las funciones están bien documentadas y son fáciles de usar. Se pueden utilizar para resolver una amplia variedad de problemas de programación.

Aquí hay algunos ejemplos de cómo se pueden usar estas funciones:

* Para calcular el factorial de un número, puedes usar la función `factorial(n)`. Por ejemplo, `factorial(5)` devolverá `120`.
* Para obtener la frecuencia de cada carácter en una cadena, puedes usar la función `frecuenciaCaracteres(cadena)`. Por ejemplo, `frecuenciaCaracteres("Hola")` devolverá `{H: 1, o: 1, l: 2, a: 1}`.
* Para encontrar el elemento más grande en un array, puedes usar la función `elementoMasGrande(array)`. Por ejemplo, `elementoMasGrande([1, 2, 3, 4, 5])` devolverá `5`.
* Para encontrar el elemento más pequeño en un array, puedes usar la función `elementoMasPequeño(array)`. Por ejemplo, `elementoMasPequeño([1, 2, 3, 4, 5])` devolverá `1`.
* Para calcular la media de los elementos de un array, puedes usar la función `media(array)`. Por ejemplo, `media([1, 2, 3, 4, 5])` devolverá `3`.
* Para calcular la mediana de los elementos de un array, puedes usar la función `mediana(array)`. Por ejemplo, `mediana([1, 2, 3, 4, 5])` devolverá `3`.
* Para calcular la moda de los elementos de un array, puedes usar la función `moda(array)`. Por ejemplo, `moda([1, 2, 3, 4, 5, 5])` devolverá `5`.
* Para encontrar la intersección de dos arrays, puedes usar la función `interseccion(array1, array2)`. Por ejemplo, `interseccion([1, 2, 3, 4, 5], [3, 4, 5, 6, 7])` devolverá `[3, 4, 5]`.
* Para encontrar la unión de dos arrays, puedes usar la función `union(array1, array2)`. Por ejemplo, `union([1, 2, 3, 4, 5], [3, 4, 5, 6, 7])` devolverá `[1, 2, 3, 4, 5, 6, 7]`.
* Para ordenar un array de menor a mayor, puedes usar la función `ordenar(array)`. Por ejemplo, `ordenar([5, 1, 3, 2, 4])` devolverá `[1, 2, 3, 4, 5]`.
* Para ordenar un array de mayor a menor, puedes usar la función `ordenarDescendente(array)`. Por ejemplo, `ordenarDescendente([5, 1, 3, 2, 4])` devolverá `[5, 4, 3, 2, 1]`.
* Para eliminar los elementos repetidos de un array, puedes usar la función `eliminarRepetidos(array)`. Por ejemplo, `eliminarRepetidos([1, 2, 3, 4, 5, 1, 2, 3])` devolverá `[1, 2, 3, 4, 5]`.
* Para invertir un array, puedes usar la función `invertir(array)`. Por ejemplo, `invertir([1, 2, 3, 4, 5])` devolverá `[5, 4, 3, 2, 1]`.