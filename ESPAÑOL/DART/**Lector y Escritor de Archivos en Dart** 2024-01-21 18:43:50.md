```dart
import 'dart:io';

// Función principal.
void main() {
  // Crear un objeto File con el nombre especificado.
  var file = new File('mi_archivo.txt');

  // Comprobar si el archivo existe.
  if (await file.exists()) {
    // Leer el contenido del archivo.
    var contenido = await file.readAsString();

    // Mostrar el contenido del archivo en la consola.
    print(contenido);
  } else {
    // Crear el archivo y escribir el contenido en él.
    await file.create(recursive: true);
    await file.writeAsString('Hola, mundo!');

    // Mostrar un mensaje de confirmación en la consola.
    print('Archivo creado');
  }
}
```

Explicación del código:

* **import 'dart:io';**: Esta línea importa la biblioteca `dart:io`, que proporciona acceso a las funcionalidades de entrada y salida de datos.
* **var file = new File('mi_archivo.txt');**: Esta línea crea un objeto `File` con el nombre especificado. El constructor `File()` toma el nombre del archivo como argumento.
* **if (await file.exists()) { ... }**: Esta línea comprueba si el archivo existe. La función `exists()` devuelve un valor booleano que indica si el archivo existe o no. La palabra clave `await` se utiliza para esperar a que la función `exists()` se ejecute antes de continuar con el código.
* **var contenido = await file.readAsString();**: Esta línea lee el contenido del archivo. La función `readAsString()` devuelve el contenido del archivo como una cadena de caracteres. La palabra clave `await` se utiliza para esperar a que la función `readAsString()` se ejecute antes de continuar con el código.
* **print(contenido);**: Esta línea muestra el contenido del archivo en la consola. La función `print()` toma un objeto como argumento y lo muestra en la consola.
* **await file.create(recursive: true);**: Esta línea crea el archivo. La función `create()` toma un argumento `recursive` que indica si la función debe crear los directorios necesarios para el archivo.
* **await file.writeAsString('Hola, mundo!');**: Esta línea escribe el contenido en el archivo. La función `writeAsString()` toma un argumento que es el contenido que se debe escribir en el archivo.
* **print('Archivo creado');**: Esta línea muestra un mensaje de confirmación en la consola. La función `print()` toma un objeto como argumento y lo muestra en la consola.