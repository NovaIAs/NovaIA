```dart
import 'dart:async';
import 'dart:io';

void main() async {
  // Crear un servidor HTTP simple
  HttpServer server = await HttpServer.bind('localhost', 8080);

  // Manejar solicitudes entrantes
  server.listen((HttpRequest request) async {
    // Obtener la ruta de la solicitud
    String path = request.uri.path;

    // Responder con el contenido correspondiente a la ruta
    if (path == '/') {
      // Página de inicio
      request.response.write('<h1>Página de inicio</h1>');
    } else if (path == '/about') {
      // Página Acerca de
      request.response.write('<h1>Acerca de</h1>');
    } else if (path == '/contacto') {
      // Página Contacto
      request.response.write('<h1>Contacto</h1>');
    } else {
      // Página no encontrada
      request.response.statusCode = 404;
      request.response.write('<h1>Página no encontrada</h1>');
    }

    // Enviar la respuesta
    await request.response.close();
  });

  // Esperar a que el servidor se cierre
  await server.close();
}
```

Explicación del código:

1. **Importar bibliotecas:**

   - `import 'dart:async';`: Importa la biblioteca `async` que proporciona clases y funciones para la programación asincrónica.
   - `import 'dart:io';`: Importa la biblioteca `io` que proporciona clases y funciones para trabajar con archivos y redes.

2. **Crear el servidor HTTP:**

   - `HttpServer server = await HttpServer.bind('localhost', 8080);`: Esta línea crea un servidor HTTP y lo vincula a la dirección IP local `'localhost'` y al puerto `8080`. El servidor se almacena en la variable `server`.

3. **Manejar solicitudes entrantes:**

   - `server.listen((HttpRequest request) async {...});`: Esta línea define un manejador de eventos para el servidor. Cuando llega una solicitud HTTP, se llama a esta función con la solicitud como argumento.

4. **Obtener la ruta de la solicitud:**

   - `String path = request.uri.path;`: Esta línea obtiene la ruta de la solicitud, que es la parte de la URL después del nombre de dominio.

5. **Responder con el contenido correspondiente a la ruta:**

   - `request.response.write('<h1>Página de inicio</h1>');`: Esta línea escribe el contenido de la página de inicio en la respuesta.
   - `request.response.write('<h1>Acerca de</h1>');`: Esta línea escribe el contenido de la página Acerca de en la respuesta.
   - `request.response.write('<h1>Contacto</h1>');`: Esta línea escribe el contenido de la página Contacto en la respuesta.
   - `request.response.statusCode = 404;` y `request.response.write('<h1>Página no encontrada</h1>');`: Estas líneas configuran el código de estado de la respuesta a 404 (Página no encontrada) y escriben un mensaje de error en la respuesta.

6. **Enviar la respuesta:**

   - `await request.response.close();`: Esta línea envía la respuesta al cliente y espera a que se cierre.

7. **Esperar a que el servidor se cierre:**

   - `await server.close();`: Esta línea espera a que el servidor se cierre antes de continuar.