```dart
// Importamos la biblioteca 'dart:io' para trabajar con archivos.
import 'dart:io';

// Definimos la función principal del programa.
void main() {
  // Creamos un archivo llamado 'mi_archivo.txt'.
  File file = File('mi_archivo.txt');

  // Escribimos en el archivo el mensaje 'Hola mundo!'.
  file.writeAsString('¡Hola mundo!');

  // Leemos el contenido del archivo y lo imprimimos en la consola.
  file.readAsString().then((contents) {
    print(contents);
  });
}
```

Explicación del código:

1. Importamos la biblioteca 'dart:io' para trabajar con archivos. Esta biblioteca nos proporciona las clases y funciones necesarias para trabajar con archivos en Dart.
2. Definimos la función principal del programa. Esta función es el punto de entrada del programa y se ejecuta cuando se inicia el programa.
3. Creamos un archivo llamado 'mi_archivo.txt'. Utilizamos la clase 'File' para crear un nuevo archivo. El constructor de la clase 'File' recibe como argumento la ruta del archivo que queremos crear. En este caso, la ruta es 'mi_archivo.txt'.
4. Escribimos en el archivo el mensaje '¡Hola mundo!'. Utilizamos el método 'writeAsString' de la clase 'File' para escribir una cadena de texto en el archivo. El método 'writeAsString' recibe como argumento la cadena de texto que queremos escribir en el archivo. En este caso, la cadena de texto es '¡Hola mundo!'.
5. Leemos el contenido del archivo y lo imprimimos en la consola. Utilizamos el método 'readAsString' de la clase 'File' para leer el contenido del archivo. El método 'readAsString' devuelve una promesa (una instancia de la clase 'Future'). Cuando la promesa se resuelve, se ejecuta la función que se pasa como argumento al método 'then'. En este caso, la función que se pasa como argumento al método 'then' imprime el contenido del archivo en la consola.

Este código es un ejemplo de cómo trabajar con archivos en Dart. Es un código sencillo, pero nos permite realizar operaciones básicas con archivos, como crear un archivo, escribir en un archivo y leer el contenido de un archivo.