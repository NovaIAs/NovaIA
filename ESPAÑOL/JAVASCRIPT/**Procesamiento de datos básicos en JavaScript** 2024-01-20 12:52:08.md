```javascript
// Definimos una función llamada "generarNumeroAleatorio" que devuelve un número aleatorio entre un rango especificado.
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Definimos una función llamada "ordenarArray" que ordena un array de números en orden ascendente.
function ordenarArray(array) {
  return array.sort((a, b) => a - b);
}

// Definimos una función llamada "buscarElemento" que busca un elemento en un array y devuelve su índice si lo encuentra.
function buscarElemento(array, elemento) {
  return array.indexOf(elemento);
}

// Definimos una función llamada "calcularPromedio" que calcula el promedio de los valores en un array de números.
function calcularPromedio(array) {
  return array.reduce((a, b) => a + b) / array.length;
}

// Definimos una función llamada "crearTablaHTML" que crea una tabla HTML a partir de un array de datos.
function crearTablaHTML(array) {
  // Creamos una variable llamada "tabla" para almacenar el código HTML de la tabla.
  let tabla = "<table>";

  // Recorremos el array de datos con un bucle for.
  for (let i = 0; i < array.length; i++) {
    // Creamos una nueva fila para cada elemento del array.
    tabla += "<tr>";

    // Recorremos el elemento actual del array con un bucle for-in.
    for (let propiedad in array[i]) {
      // Añadimos una nueva columna para cada propiedad del elemento.
      tabla += "<td>" + array[i][propiedad] + "</td>";
    }

    // Cerramos la fila.
    tabla += "</tr>";
  }

  // Cerramos la tabla.
  tabla += "</table>";

  // Devolvemos el código HTML de la tabla.
  return tabla;
}

// Creamos un array de números aleatorios.
const numerosAleatorios = [];
for (let i = 0; i < 10; i++) {
  numerosAleatorios.push(generarNumeroAleatorio(1, 100));
}

// Ordenamos el array de números aleatorios.
const numerosOrdenados = ordenarArray(numerosAleatorios);

// Buscamos el elemento 50 en el array de números ordenados.
const indiceDe50 = buscarElemento(numerosOrdenados, 50);

// Calculamos el promedio de los valores en el array de números aleatorios.
const promedio = calcularPromedio(numerosAleatorios);

// Creamos una tabla HTML a partir del array de números aleatorios.
const tablaHTML = crearTablaHTML(numerosAleatorios);

// Mostramos los resultados en la consola.
console.log("Números aleatorios:", numerosAleatorios);
console.log("Números ordenados:", numerosOrdenados);
console.log("Índice de 50:", indiceDe50);
console.log("Promedio:", promedio);
console.log("Tabla HTML:", tablaHTML);
```

Explicación del código:

* La función `generarNumeroAleatorio` utiliza la función `Math.random()` para generar un número aleatorio entre un rango especificado.
* La función `ordenarArray` utiliza el método `sort()` del array para ordenar los elementos en orden ascendente.
* La función `buscarElemento` utiliza el método `indexOf()` del array para buscar un elemento en el array y devolver su índice si lo encuentra.
* La función `calcularPromedio` utiliza el método `reduce()` del array para calcular el promedio de los valores en el array.
* La función `crearTablaHTML` utiliza un bucle for para recorrer un array de datos y crear una tabla HTML a partir de los datos.
* En el código principal, primero creamos un array de números aleatorios utilizando la función `generarNumeroAleatorio`.
* Luego ordenamos el array de números aleatorios utilizando la función `ordenarArray`.
* Buscamos el elemento 50 en el array de números ordenados utilizando la función `buscarElemento`.
* Calculamos el promedio de los valores en el array de números aleatorios utilizando la función `calcularPromedio`.
* Creamos una tabla HTML a partir del array de números aleatorios utilizando la función `crearTablaHTML`.
* Finalmente, mostramos los resultados en la consola.