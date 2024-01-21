```dart
// Importamos las bibliotecas necesarias.

import 'dart:io';
import 'dart:async';
import 'package:path/path.dart';
import 'package:http/http.dart' as http;
import 'package:file_picker/file_picker.dart';

// **Función principal**

Future<void> main() async {
  // Creamos un servidor HTTP.

  var server = await HttpServer.bind('127.0.0.1', 8080);

  // **Función que maneja las solicitudes del servidor.**

  server.listen((HttpRequest request) async {
    // Obtenemos la ruta de la solicitud.

    var path = request.uri.path;

    // **Ruta raíz**

    if (path == '/') {
      var response = File(join(Directory.current.path, 'index.html')).readAsStringSync();
      request.response.write(response);
    }

    // **Ruta para subir archivos**

    else if (path == '/upload') {
      // Obtenemos el archivo subido.

      var file = await request.receiveBytes();

      // Guardamos el archivo en el sistema de archivos.

      var filename = basename(request.headers['Content-Disposition']).substring(11, -1);
      var fileBytes = file;
      var fileToSave = File(join(Directory.current.path, filename));
      await fileToSave.writeAsBytes(fileBytes);

      // Respondemos al cliente con un código de estado 200.

      request.response.statusCode = HttpStatus.ok;
    }

    // **Cualquier otra ruta**

    else {
      // Respondemos al cliente con un código de estado 404.

      request.response.statusCode = HttpStatus.notFound;
    }

    // Cerramos la respuesta.

    request.response.close();
  });

  // Imprimimos un mensaje en la consola.

  print('Servidor escuchando en el puerto 8080.');
}
```

**Explicación del código:**

* El código crea un servidor HTTP que escucha en el puerto 8080.
* El servidor tiene dos rutas: una ruta raíz que muestra el archivo `index.html` y una ruta `/upload` que permite subir archivos.
* Cuando un cliente realiza una solicitud a la ruta raíz, el servidor responde con el contenido del archivo `index.html`.
* Cuando un cliente realiza una solicitud a la ruta `/upload`, el servidor recibe el archivo subido y lo guarda en el sistema de archivos.
* El servidor responde al cliente con un código de estado 200 si la subida del archivo se realiza correctamente.
* Si el cliente realiza una solicitud a cualquier otra ruta, el servidor responde con un código de estado 404.