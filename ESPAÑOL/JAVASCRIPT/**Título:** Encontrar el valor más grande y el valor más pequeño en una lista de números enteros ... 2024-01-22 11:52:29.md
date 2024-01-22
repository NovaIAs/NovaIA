**Problema:**
Dada una lista de números enteros, encontrar el valor más grande y el valor más pequeño en la lista.

**Solución:**
```javascript
// Función para encontrar el valor más grande y el valor más pequeño en una lista de números enteros
function encontrarMinMax(lista) {
  // Si la lista está vacía, devuelve null
  if (lista.length === 0) {
    return null;
  }

  // Inicializar el valor más grande y el valor más pequeño con el primer elemento de la lista
  let max = lista[0];
  let min = lista[0];

  // Recorrer la lista y actualizar el valor más grande y el valor más pequeño según sea necesario
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] > max) {
      max = lista[i];
    } else if (lista[i] < min) {
      min = lista[i];
    }
  }

  // Devolver el valor más grande y el valor más pequeño en un objeto
  return {
    max: max,
    min: min
  };
}

// Ejemplo de uso
const lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const resultado = encontrarMinMax(lista);
console.log(resultado); // { max: 10, min: 1 }
```

Este código funciona de la siguiente manera:

1. La función `encontrarMinMax()` toma una lista de números enteros como argumento.
2. Si la lista está vacía, la función devuelve `null`.
3. La función inicializa el valor más grande y el valor más pequeño con el primer elemento de la lista.
4. La función recorre la lista y actualiza el valor más grande y el valor más pequeño según sea necesario.
5. La función devuelve el valor más grande y el valor más pequeño en un objeto.
6. El código de ejemplo crea una lista de números enteros y llama a la función `encontrarMinMax()` para encontrar el valor más grande y el valor más pequeño en la lista.
7. El resultado se imprime en la consola.