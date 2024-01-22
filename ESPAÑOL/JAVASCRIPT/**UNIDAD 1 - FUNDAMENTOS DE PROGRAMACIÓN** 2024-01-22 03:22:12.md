```javascript
// Función principal
const crearSaludo = (nombre, apellido) => {
  // Comprobar si los parámetros son válidos
  if (!nombre || !apellido) {
    throw new Error("Los parámetros nombre y apellido son obligatorios");
  }

  // Crear el saludo
  const saludo = `Hola, ${nombre} ${apellido}!`;

  // Devolver el saludo
  return saludo;
};

// Función auxiliar para generar un número aleatorio
const generarNumeroAleatorio = (min, max) => {
  // Comprobar si los parámetros son válidos
  if (min > max) {
    throw new Error("El valor mínimo no puede ser mayor que el valor máximo");
  }

  // Generar un número aleatorio entre el mínimo y el máximo
  const numeroAleatorio = Math.floor(Math.random() * (max - min + 1)) + min;

  // Devolver el número aleatorio
  return numeroAleatorio;
};

// Función auxiliar para generar una lista de números aleatorios
const generarListaNumerosAleatorios = (longitud, min, max) => {
  // Comprobar si los parámetros son válidos
  if (longitud <= 0) {
    throw new Error("La longitud de la lista debe ser mayor que 0");
  }

  if (min > max) {
    throw new Error("El valor mínimo no puede ser mayor que el valor máximo");
  }

  // Crear una lista vacía
  const listaNumerosAleatorios = [];

  // Generar un número aleatorio para cada posición de la lista
  for (let i = 0; i < longitud; i++) {
    const numeroAleatorio = generarNumeroAleatorio(min, max);
    listaNumerosAleatorios.push(numeroAleatorio);
  }

  // Devolver la lista de números aleatorios
  return listaNumerosAleatorios;
};

// Función auxiliar para calcular la media de una lista de números
const calcularMedia = (listaNumeros) => {
  // Comprobar si la lista de números es válida
  if (!listaNumeros || listaNumeros.length === 0) {
    throw new Error("La lista de números no puede ser nula o vacía");
  }

  // Calcular la suma de los números de la lista
  let suma = 0;
  for (let i = 0; i < listaNumeros.length; i++) {
    suma += listaNumeros[i];
  }

  // Calcular la media de los números de la lista
  const media = suma / listaNumeros.length;

  // Devolver la media
  return media;
};

// Función principal
const main = () => {
  // Pedir el nombre y el apellido al usuario
  const nombre = prompt("¿Cuál es tu nombre?");
  const apellido = prompt("¿Cuál es tu apellido?");

  // Crear un saludo personalizado para el usuario
  const saludo = crearSaludo(nombre, apellido);

  // Mostrar el saludo en la consola
  console.log(saludo);

  // Generar una lista de 10 números aleatorios entre 1 y 100
  const listaNumerosAleatorios = generarListaNumerosAleatorios(10, 1, 100);

  // Mostrar la lista de números aleatorios en la consola
  console.log(listaNumerosAleatorios);

  // Calcular la media de la lista de números aleatorios
  const media = calcularMedia(listaNumerosAleatorios);

  // Mostrar la media en la consola
  console.log(`La media de la lista de números aleatorios es ${media}`);
};

// Llamar a la función principal
main();
```

Explicación del código:
- Función crearSaludo: esta función recibe dos argumentos, el nombre y el apellido, y devuelve un saludo personalizado.
- Función generarNumeroAleatorio: esta función recibe dos argumentos, el mínimo y el máximo, y devuelve un número aleatorio entre esos valores.
- Función generarListaNumerosAleatorios: esta función recibe tres argumentos, la longitud de la lista, el mínimo y el máximo, y devuelve una lista de números aleatorios entre esos valores.
- Función calcularMedia: esta función recibe una lista de números y devuelve la media de esos números.
- Función main: esta función se llama al final del código y es la que ejecuta el resto de funciones. Esta función pide el nombre y el apellido al usuario, crea un saludo personalizado, genera una lista de números aleatorios, calcula la media de la lista de números aleatorios y muestra todos estos datos en la consola.