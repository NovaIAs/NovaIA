```dart
// Importamos las bibliotecas necesarias.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Creamos un cliente HTTP para realizar peticiones a la API.
final httpClient = HttpClient();

// Definimos la URL de la API.
final apiUrl = 'https://api.example.com';

// Creamos una función para obtener los datos de la API.
Future<Map<String, dynamic>> getApiData() async {
  // Hacemos una petición GET a la API.
  final request = await httpClient.getUrl(Uri.parse(apiUrl));

  // Obtenemos la respuesta de la API.
  final response = await request.close();

  // Leemos el cuerpo de la respuesta.
  final body = await response.transform(utf8.decoder).join();

  // Convertimos el cuerpo de la respuesta en un mapa JSON.
  final data = json.decode(body) as Map<String, dynamic>;

  // Devolvemos los datos de la API.
  return data;
}

// Creamos una función para mostrar los datos de la API.
void showApiData(Map<String, dynamic> data) {
  // Mostramos los datos de la API en la consola.
  print(data);
}

// Llamamos a la función para obtener los datos de la API.
getApiData().then((data) {
  // Llamamos a la función para mostrar los datos de la API.
  showApiData(data);
});
```

Explicación del código:

* Importamos las bibliotecas necesarias, como `dart:async`, `dart:convert` e `dart:io`.
* Creamos un cliente HTTP para realizar peticiones a la API.
* Definimos la URL de la API.
* Creamos una función para obtener los datos de la API, que realiza una petición GET a la API y devuelve los datos de la API en formato JSON.
* Creamos una función para mostrar los datos de la API en la consola.
* Llamamos a la función para obtener los datos de la API y luego llamamos a la función para mostrar los datos de la API en la consola.