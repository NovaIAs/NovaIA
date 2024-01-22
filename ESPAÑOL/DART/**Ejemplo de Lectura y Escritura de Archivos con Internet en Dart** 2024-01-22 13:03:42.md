```dart
// Importamos las librerías necesarias.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Definimos una función para manejar las excepciones.
void manejarExcepciones(Exception e) {
  print('Ocurrió un error: $e');
}

// Definimos una función para leer un archivo.
Future<String> leerArchivo(String ruta) async {
  try {
    // Leemos el archivo.
    final file = File(ruta);
    final contents = await file.readAsString();

    // Devolvemos el contenido del archivo.
    return contents;
  } catch (e) {
    // Manejamos la excepción.
    manejarExcepciones(e);

    // Devolvemos una cadena vacía.
    return '';
  }
}

// Definimos una función para escribir un archivo.
Future<void> escribirArchivo(String ruta, String contenido) async {
  try {
    // Creamos el archivo.
    final file = File(ruta);
    await file.writeAsString(contenido);
  } catch (e) {
    // Manejamos la excepción.
    manejarExcepciones(e);
  }
}

// Definimos una función para realizar una petición HTTP.
Future<String> hacerPeticionHTTP(String url) async {
  try {
    // Creamos el cliente HTTP.
    final client = HttpClient();

    // Realizamos la petición HTTP.
    final request = await client.getUrl(Uri.parse(url));
    final response = await request.close();

    // Leemos la respuesta HTTP.
    final contents = await response.transform(utf8.decoder).join();

    // Devolvemos el contenido de la respuesta HTTP.
    return contents;
  } catch (e) {
    // Manejamos la excepción.
    manejarExcepciones(e);

    // Devolvemos una cadena vacía.
    return '';
  }
}

// Definimos la función principal.
void main() async {
  // Leemos el archivo de configuración.
  final configuracion = await leerArchivo('configuracion.json');

  // Convertimos el archivo de configuración a un objeto JSON.
  final json = jsonDecode(configuracion);

  // Obtenemos la URL del servicio web.
  final url = json['servicio_web'];

  // Realizamos la petición HTTP.
  final respuesta = await hacerPeticionHTTP(url);

  // Convertimos la respuesta HTTP a un objeto JSON.
  final jsonRespuesta = jsonDecode(respuesta);

  // Obtenemos el mensaje de la respuesta HTTP.
  final mensaje = jsonRespuesta['mensaje'];

  // Mostramos el mensaje de la respuesta HTTP.
  print(mensaje);

  // Escribimos el mensaje de la respuesta HTTP en un archivo.
  await escribirArchivo('respuesta.txt', mensaje);

  // Cerramos el cliente HTTP.
  client.close();
}
```

Explicación del código:

* Importamos las librerías necesarias.
* Definimos una función para manejar las excepciones.
* Definimos una función para leer un archivo.
* Definimos una función para escribir un archivo.
* Definimos una función para realizar una petición HTTP.
* Definimos la función principal.
* Leemos el archivo de configuración.
* Convertimos el archivo de configuración a un objeto JSON.
* Obtenemos la URL del servicio web.
* Realizamos la petición HTTP.
* Convertimos la respuesta HTTP a un objeto JSON.
* Obtenemos el mensaje de la respuesta HTTP.
* Mostramos el mensaje de la respuesta HTTP.
* Escribimos el mensaje de la respuesta HTTP en un archivo.
* Cerramos el cliente HTTP.