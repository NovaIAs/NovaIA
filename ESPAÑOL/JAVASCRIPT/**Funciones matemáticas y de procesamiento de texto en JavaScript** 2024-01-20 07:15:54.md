```javascript
// CREAR UNA FUNCIÓN QUE RECIBA UNA LISTA DE NÚMEROS Y DEVUELVA LA SUMA DE TODOS LOS NÚMEROS PARES.
function sumaPares(listaNumeros) {
  // SE CREA UNA VARIABLE PARA ALMACENAR EL RESULTADO DE LA SUMA.
  let sumaPares = 0;

  // SE RECORRE LA LISTA DE NÚMEROS CON UN FOR.
  for (let i = 0; i < listaNumeros.length; i++) {
    // SE COMPARA SI EL NÚMERO ACTUAL ES PAR.
    if (listaNumeros[i] % 2 === 0) {
      // SI ES PAR, SE AGREGA A LA SUMA PARES.
      sumaPares += listaNumeros[i];
    }
  }

  // DEVOLVER LA SUMA PARES.
  return sumaPares;
}

// EJEMPLO DE USO DE LA FUNCIÓN.
const listaNumeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const sumaParesLista = sumaPares(listaNumeros);
console.log("La suma de los números pares en la lista es:", sumaParesLista); // IMPRIMIR LA SUMA EN LA CONSOLA.

// CREAR UNA FUNCIÓN QUE RECIBA UN NÚMERO Y DEVUELVA SU FACTORIAL.
function factorial(numero) {
  // SE CREA UNA VARIABLE PARA ALMACENAR EL RESULTADO DEL FACTORIAL.
  let factorial = 1;

  // SE RECORRE EL NÚMERO CON UN FOR.
  for (let i = 1; i <= numero; i++) {
    // SE MULTIPLICA EL FACTORIAL POR EL NÚMERO ACTUAL.
    factorial *= i;
  }

  // DEVOLVER EL FACTORIAL.
  return factorial;
}

// EJEMPLO DE USO DE LA FUNCIÓN.
const numero = 5;
const factorialNumero = factorial(numero);
console.log("El factorial de", numero, "es:", factorialNumero); // IMPRIMIR EL FACTORIAL EN LA CONSOLA.

// CREAR UNA FUNCIÓN QUE RECIBA UNA CADENA Y DEVUELVA UN ARRAY CON LAS PALABRAS DE LA CADENA.
function separarPalabras(cadena) {
  // SE CREA UN ARRAY PARA ALMACENAR LAS PALABRAS.
  const palabras = [];

  // SE SEPARA LA CADENA EN PALABRAS UTILIZANDO EL MÉTODO split().
  const palabrasSeparadas = cadena.split(" ");

  // SE RECORREN LAS PALABRAS SEPARADAS CON UN FOR.
  for (let i = 0; i < palabrasSeparadas.length; i++) {
    // SE LIMPIA CADA PALABRA DE ESPACIOS EN BLANCO UTILIZANDO EL MÉTODO trim().
    const palabraLimpia = palabrasSeparadas[i].trim();

    // SI LA PALABRA LIMPIA NO ESTÁ VACÍA, SE AGREGA AL ARRAY DE PALABRAS.
    if (palabraLimpia !== "") {
      palabras.push(palabraLimpia);
    }
  }

  // DEVOLVER EL ARRAY DE PALABRAS.
  return palabras;
}

// EJEMPLO DE USO DE LA FUNCIÓN.
const cadena = "Hola mundo, cómo estás?";
const palabrasSeparadas = separarPalabras(cadena);
console.log("Las palabras de la cadena son:", palabrasSeparadas); // IMPRIMIR LAS PALABRAS EN LA CONSOLA.
```

El código anterior contiene tres funciones complejas:

* `sumaPares` suma todos los números pares en una lista.
* `factorial` calcula el factorial de un número.
* `separarPalabras` separa una cadena en un array de palabras.

Cada función está bien documentada con comentarios en español y también se incluyen ejemplos de uso de las funciones.