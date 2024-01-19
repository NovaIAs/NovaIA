```javascript
// Definición de la función principal
function calcularHipotenusa(cateto1, cateto2) {
  // Comprobar si los parámetros son válidos
  if (cateto1 <= 0 || cateto2 <= 0) {
    throw new Error("Los catetos deben ser positivos");
  }

  // Calcular la hipotenusa utilizando el teorema de Pitágoras
  const hipotenusa = Math.sqrt(Math.pow(cateto1, 2) + Math.pow(cateto2, 2));

  // Redondear la hipotenusa a dos decimales
  return hipotenusa.toFixed(2);
}

// Definición de la función para pedir datos al usuario
function pedirDatos() {
  // Obtener los valores introducidos por el usuario
  const cateto1 = prompt("Introduce el valor del primer cateto:");
  const cateto2 = prompt("Introduce el valor del segundo cateto:");

  // Convertir los valores a números
  const numeroCateto1 = Number(cateto1);
  const numeroCateto2 = Number(cateto2);

  // Devolver los valores convertidos
  return [numeroCateto1, numeroCateto2];
}

// Definición de la función para mostrar el resultado
function mostrarResultado(hipotenusa) {
  // Crear un elemento de párrafo para mostrar el resultado
  const parrafo = document.createElement("p");

  // Añadir el texto del resultado al párrafo
  parrafo.textContent = `La hipotenusa es: ${hipotenusa}`;

  // Añadir el párrafo al documento
  document.body.appendChild(parrafo);
}

// Obtener los valores introducidos por el usuario
const [cateto1, cateto2] = pedirDatos();

// Calcular la hipotenusa
const hipotenusa = calcularHipotenusa(cateto1, cateto2);

// Mostrar el resultado
mostrarResultado(hipotenusa);
```

Explicación del código:

* La función `calcularHipotenusa` calcula la hipotenusa de un triángulo rectángulo dados los valores de sus dos catetos.
* La función `pedirDatos` pide al usuario que introduzca los valores de los dos catetos y los devuelve en un array.
* La función `mostrarResultado` crea un elemento de párrafo en el documento HTML y lo añade al cuerpo del documento, mostrando el valor de la hipotenusa.
* La función `main` ejecuta el programa, llamando a las funciones anteriores y mostrando el resultado al usuario.