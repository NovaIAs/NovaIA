```dart
import 'dart:io';
import 'dart:async';
import 'dart:convert';
import 'package:path/path.dart';

main(List<String> arguments) async {
  // Comprueba si se proporcionan argumentos válidos
  if (arguments.length < 2) {
    print("Uso: <comando> <fichero entrada> <fichero salida>");
    return;
  }

  // Obtener el nombre de los ficheros de entrada y salida
  String inputFile = arguments[0];
  String outputFile = arguments[1];

  // Leer el fichero de entrada
  String inputText = await File(inputFile).readAsString();

  // Crear un temporizador para medir el tiempo de procesamiento
  Stopwatch timer = Stopwatch()..start();

  // Procesar el texto de entrada y generar el texto de salida
  String outputText = '';
  for (String line in inputText.split('\n')) {
    if (line.isNotEmpty) {
      // Procesar cada línea
      String processedLine = line.toUpperCase();
      outputText += processedLine + '\n';
    }
  }

  // Escribir el texto de salida en el fichero de salida
  await File(outputFile).writeAsString(outputText);

  // Detener el temporizador y mostrar el tiempo de procesamiento
  timer.stop();
  print("Tiempo de procesamiento: ${timer.elapsedMilliseconds} milisegundos");
}
```

Explicación del código:

* El código anterior es un programa que lee un fichero de entrada, procesa su contenido y escribe el resultado en un fichero de salida.
* La función `main` es la función principal del programa y se ejecuta al iniciar el programa.
* Se utilizan los paquetes `dart:io`, `dart:async` y `package:path/path.dart` para la entrada/salida de ficheros y la manipulación de rutas de ficheros.
* Se utiliza el temporizador `Stopwatch` para medir el tiempo de procesamiento.
* El programa utiliza un ciclo `for` para procesar cada línea del fichero de entrada y escribir el resultado en el fichero de salida.
* En este ejemplo, el procesamiento consiste en convertir cada línea a mayúsculas.
* El programa muestra el tiempo de procesamiento una vez que se ha completado.