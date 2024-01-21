```javascript
// Definimos una función llamada saludar que recibe un nombre como parámetro
function saludar(nombre) {
  // Utilizamos la estructura condicional if/else para comprobar si el nombre es una cadena vacía
  if (nombre === "") {
    // Si el nombre es una cadena vacía, mostramos un mensaje de error
    console.error("No se ha proporcionado ningún nombre");
  } else {
    // Si el nombre no es una cadena vacía, mostramos un mensaje de saludo
    console.log(`Hola, ${nombre}!`);
  }
}

// Llamamos a la función saludar con diferentes nombres
saludar("Juan"); // Hola, Juan!
saludar("María"); // Hola, María!
saludar(""); // No se ha proporcionado ningún nombre

// Definimos una función llamada calcularAreaTriangulo que recibe la base y la altura del triángulo como parámetros
function calcularAreaTriangulo(base, altura) {
  // Comprobamos si la base y la altura son números válidos
  if (typeof base !== "number" || typeof altura !== "number") {
    // Si alguno de los parámetros no es un número, mostramos un mensaje de error
    console.error("Los parámetros deben ser números");
  } else if (base <= 0 || altura <= 0) {
    // Si la base o la altura son menores o iguales a 0, mostramos un mensaje de error
    console.error("La base y la altura deben ser positivas");
  } else {
    // Si la base y la altura son números válidos y positivos, calculamos el área del triángulo
    const area = (base * altura) / 2;
    // Mostramos el área del triángulo
    console.log(`El área del triángulo es ${area} unidades cuadradas`);
  }
}

// Llamamos a la función calcularAreaTriangulo con diferentes valores
calcularAreaTriangulo(10, 5); // El área del triángulo es 25 unidades cuadradas
calcularAreaTriangulo(15, 12); // El área del triángulo es 90 unidades cuadradas
calcularAreaTriangulo(20, 0); // La base y la altura deben ser positivas
calcularAreaTriangulo("10", "5"); // Los parámetros deben ser números

// Definimos una función llamada factorial que recibe un número como parámetro
function factorial(numero) {
  // Comprobamos si el número es un número válido
  if (typeof numero !== "number") {
    // Si el número no es un número, mostramos un mensaje de error
    console.error("El parámetro debe ser un número");
  } else if (numero < 0) {
    // Si el número es menor que 0, mostramos un mensaje de error
    console.error("El número debe ser positivo");
  } else if (numero > 170) {
    // Si el número es mayor que 170, mostramos un mensaje de error
    console.error("El número es demasiado grande");
  } else {
    // Si el número es un número válido, calculamos el factorial
    let factorial = 1;
    for (let i = 2; i <= numero; i++) {
      factorial *= i;
    }
    // Mostramos el factorial del número
    console.log(`El factorial de ${numero} es ${factorial}`);
  }
}

// Llamamos a la función factorial con diferentes valores
factorial(5); // El factorial de 5 es 120
factorial(10); // El factorial de 10 es 3628800
factorial(-5); // El número debe ser positivo
factorial("10"); // El parámetro debe ser un número
factorial(171); // El número es demasiado grande
```

Este código es un conjunto de tres funciones complejas que realizan diferentes tareas:

* La función `saludar` recibe un nombre como parámetro y muestra un mensaje de saludo.
* La función `calcularAreaTriangulo` recibe la base y la altura de un triángulo como parámetros y calcula el área del triángulo.
* La función `factorial` recibe un número como parámetro y calcula el factorial del número.

Este código es bastante complejo, pero está bien documentado y comentado, por lo que es fácil entender cómo funciona.