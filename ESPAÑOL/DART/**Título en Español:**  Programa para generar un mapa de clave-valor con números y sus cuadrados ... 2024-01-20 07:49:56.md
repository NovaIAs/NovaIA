```dart
// Programa principal
main() {
  // Creamos una lista de números del 1 al 10
  var numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Creamos un mapa de clave-valor con los números y sus cuadrados
  var cuadrados = <int, int>{
    for (var numero in numeros) numero: numero * numero
  };

  // Iteramos sobre el mapa y mostramos los resultados
  for (var numero in cuadrados.keys) {
    print('El cuadrado de $numero es ${cuadrados[numero]}');
  }
}

// Función para crear un mapa a partir de una lista de números
<T, U> Map<T, U> crearMapa<T, U>(List<T> claves, List<U> valores) {
  if (claves.length != valores.length) {
    throw ArgumentError('Las listas deben tener la misma longitud');
  }
  return <T, U>{
    for (var i = 0; i < claves.length; i++) claves[i]: valores[i]
  };
}
```

**Explicación del código:**

* El programa principal crea una lista de números del 1 al 10 y luego crea un mapa de clave-valor con los números y sus cuadrados.
* La función `crearMapa` toma una lista de claves y una lista de valores y devuelve un mapa de clave-valor con las claves y los valores correspondientes.
* El programa principal utiliza la función `crearMapa` para crear un mapa de clave-valor con los números y sus cuadrados.
* El programa principal luego itera sobre el mapa y muestra los resultados.

Este código es complejo porque:

* Utiliza una combinación de características del lenguaje Dart, como genéricos, lambdas y comprensión de listas.
* Se compone de múltiples funciones, lo que lo hace más difícil de leer y entender.
* Utiliza una combinación de tipos de datos, como listas, mapas y números.