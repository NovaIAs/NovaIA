```javascript
// Función para encontrar el mayor factor primo de un número.
function mayorFactorPrimo(numero) {
  // Inicializar el factor primo más grande.
  let factorPrimoMasGrande = 1;

  // Iterar sobre todos los números desde 2 hasta la raíz cuadrada del número dado.
  for (let i = 2; i <= Math.sqrt(numero); i++) {
    // Si el número es divisible por i sin resto, entonces i es un factor primo del número.
    if (numero % i === 0) {
      // Actualizar el factor primo más grande si i es mayor que el factor primo más grande actual.
      factorPrimoMasGrande = i;

      // Continuar dividiendo el número por i hasta que ya no sea divisible por i.
      while (numero % i === 0) {
        numero /= i;
      }
    }
  }

  // Si el número no se puede dividir por ningún número menor que su raíz cuadrada, entonces el número en sí es primo y el factor primo más grande es el número en sí.
  if (numero > 1) {
    factorPrimoMasGrande = numero;
  }

  // Devolver el factor primo más grande.
  return factorPrimoMasGrande;
}

// Ejemplo de uso de la función.
console.log(mayorFactorPrimo(600851475143)); // 6857
```

Explicación del código:

* La función `mayorFactorPrimo` toma un número como parámetro y devuelve el mayor factor primo del número.
* La función itera sobre todos los números desde 2 hasta la raíz cuadrada del número dado.
* Si el número es divisible por i sin resto, entonces i es un factor primo del número.
* La función actualiza el factor primo más grande si i es mayor que el factor primo más grande actual.
* La función continúa dividiendo el número por i hasta que ya no sea divisible por i.
* Si el número no se puede dividir por ningún número menor que su raíz cuadrada, entonces el número en sí es primo y el factor primo más grande es el número en sí.
* La función devuelve el factor primo más grande.
* El ejemplo de uso de la función muestra el mayor factor primo del número 600851475143, que es 6857.