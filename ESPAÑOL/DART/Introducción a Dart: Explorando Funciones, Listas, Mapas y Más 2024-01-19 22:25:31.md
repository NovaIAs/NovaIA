```dart
// Función principal del programa.
void main() {
  // Creamos una lista de números.
  List<int> numeros = [1, 2, 3, 4, 5];

  // Creamos un mapa de claves y valores.
  Map<String, String> mapa = {
    "nombre": "Juan",
    "apellido": "García",
    "edad": "20 años"
  };

  // Creamos una función que suma dos números.
  int suma(int a, int b) {
    return a + b;
  }

  // Creamos una función que retorna el valor máximo de una lista de números.
  int maximo(List<int> lista) {
    int max = lista[0];
    for (int i = 1; i < lista.length; i++) {
      if (lista[i] > max) {
        max = lista[i];
      }
    }
    return max;
  }

  // Imprimimos los resultados.
  print(numeros);
  print(mapa);
  print(suma(1, 2));
  print(maximo(numeros));
}
```

Explicación del código:

* La función `main()` es la función principal del programa. Es donde se ejecuta el código principal.
* La lista `numeros` contiene los números 1, 2, 3, 4 y 5.
* El mapa `mapa` contiene las claves "nombre", "apellido" y "edad" y los valores "Juan", "García" y "20 años" respectivamente.
* La función `suma()` suma dos números y retorna el resultado.
* La función `maximo()` retorna el valor máximo de una lista de números.
* Las instrucciones `print()` imprimen los resultados en la consola.