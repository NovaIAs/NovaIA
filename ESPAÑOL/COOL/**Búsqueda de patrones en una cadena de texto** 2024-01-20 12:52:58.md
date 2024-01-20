```cool
clase Búsqueda {
  método principal() {
    cadena patrón = "cadena a buscar";
    cadena texto = "texto en el que se buscará";
    posición posiciónEncontrada = texto.buscar(patrón);
    si (posiciónEncontrada >= 0) {
      escribir("¡Se encontró el patrón en la posición " + posiciónEncontrada + "!");
    } más {
      escribir("No se encontró el patrón.");
    }
  }
}
```

Explicación:

* La clase `Búsqueda` contiene un método principal que busca un patrón en un texto.
* `cadena patrón` y `cadena texto` son variables de tipo `cadena` que almacenan el patrón a buscar y el texto en el que se buscará, respectivamente.
* `posición posiciónEncontrada` es una variable de tipo `posición` que almacena la posición del patrón en el texto.
* `texto.buscar(patrón)` es un método que busca el patrón en el texto y devuelve la posición del primer carácter del patrón si se encuentra, o -1 si no se encuentra.
* `si (posiciónEncontrada >= 0)` es una instrucción `si` que comprueba si el patrón se encontró en el texto.
* `escribir("¡Se encontró el patrón en la posición " + posiciónEncontrada + "!");` es una instrucción `escribir` que imprime un mensaje indicando que el patrón se encontró en el texto.
* `más` es una instrucción `más` que se ejecuta si el patrón no se encontró en el texto.
* `escribir("No se encontró el patrón.");` es una instrucción `escribir` que imprime un mensaje indicando que el patrón no se encontró en el texto.