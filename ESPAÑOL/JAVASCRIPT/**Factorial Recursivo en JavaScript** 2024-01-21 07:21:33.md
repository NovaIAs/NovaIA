```javascript
// Función principal que recibe como parámetro un número entero.
const factorial = (n) => {
  // Si el número es menor que 0, se lanza un error.
  if (n < 0) {
    throw new Error("El número debe ser un entero no negativo.");
  }

  // Si el número es igual a 0, se devuelve 1.
  if (n === 0) {
    return 1;
  }

  // Se calcula el factorial del número llamando recursivamente a la función.
  return n * factorial(n - 1);
};

// Ejemplo de uso de la función factorial.
const resultado = factorial(5);

// Se imprime el resultado del factorial.
console.log("5! =", resultado);
```

Explicación del código:

* La función `factorial` recibe un número entero como parámetro y devuelve el factorial de ese número.
* El factorial de un número es el producto de todos los números enteros positivos menores o iguales que ese número.
* La función utiliza la recursión para calcular el factorial de un número.
* La recursión es una técnica en la que una función se llama a sí misma hasta que un caso base se alcanza.
* En este caso, el caso base es el número 0, ya que el factorial de 0 es 1.
* Si el número es mayor que 0, la función se llama a sí misma con el número menos 1.
* Este proceso se repite hasta que se alcanza el caso base.
* La función entonces devuelve el producto de todos los números enteros positivos menores o iguales que el número original.
* El ejemplo de uso de la función factorial muestra cómo calcular el factorial de 5.
* El resultado del factorial de 5 es 120.