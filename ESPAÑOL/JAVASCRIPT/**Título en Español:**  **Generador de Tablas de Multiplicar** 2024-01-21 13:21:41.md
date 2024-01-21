```javascript
const crearTablaMultiplicar = (numero) => {
  const tabla = [];

  for (let i = 1; i <= 10; i++) {
    tabla.push(`${numero} x ${i} = ${numero * i}`);
  }

  return tabla;
};

const mostrarTablaMultiplicar = (numero) => {
  const tabla = crearTablaMultiplicar(numero);

  console.log(`Tabla de multiplicar del ${numero}:`);
  tabla.forEach(fila => console.log(fila));
};

const mostrarTodasLasTablasMultiplicar = () => {
  for (let i = 1; i <= 10; i++) {
    mostrarTablaMultiplicar(i);
  }
};

mostrarTodasLasTablasMultiplicar();
```

Este código crea una tabla de multiplicar para un número dado y la muestra en la consola. También crea una función que muestra todas las tablas de multiplicar del 1 al 10.

El código se explica a continuación:

* La función `crearTablaMultiplicar` toma un número como argumento y devuelve una matriz con la tabla de multiplicar de ese número. La matriz contiene 10 filas, una para cada número del 1 al 10. Cada fila contiene una cadena con el formato "numero x i = numero * i", donde "numero" es el número dado como argumento, "i" es el número de fila y "numero * i" es el producto de "numero" e "i".
* La función `mostrarTablaMultiplicar` toma un número como argumento y muestra la tabla de multiplicar de ese número en la consola. La función llama a la función `crearTablaMultiplicar` para obtener la tabla de multiplicar y luego itera sobre ella, imprimiendo cada fila en la consola.
* La función `mostrarTodasLasTablasMultiplicar` llama a la función `mostrarTablaMultiplicar` para mostrar todas las tablas de multiplicar del 1 al 10.

El código utiliza una serie de características de JavaScript, incluyendo funciones, bucles, iteradores y cadenas de plantillas. También utiliza la función `console.log()` para imprimir datos en la consola.