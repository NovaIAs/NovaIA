```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Definir una clase para manejar las peticiones HTTP.
class HttpClient {
  // Método para realizar una petición GET.
  Future<dynamic> get(String url) async {
    // Crear un cliente HTTP.
    HttpClient client = HttpClient();

    // Realizar la petición.
    try {
      Uri uri = Uri.parse(url);
      HttpClientRequest request = await client.getUrl(uri);
      HttpClientResponse response = await request.close();

      // Leer la respuesta.
      String body = await response.transform(utf8.decoder).join();

      // Devolver la respuesta.
      return body;
    } catch (e) {
      // Manejar el error.
      throw Exception('No se pudo realizar la petición GET: $e');
    } finally {
      // Cerrar el cliente HTTP.
      client.close();
    }
  }

  // Método para realizar una petición POST.
  Future<dynamic> post(String url, Map<String, dynamic> body) async {
    // Crear un cliente HTTP.
    HttpClient client = HttpClient();

    // Realizar la petición.
    try {
      Uri uri = Uri.parse(url);
      HttpClientRequest request = await client.postUrl(uri);
      request.headers.set('Content-Type', 'application/json');
      request.add(utf8.encode(json.encode(body)));
      HttpClientResponse response = await request.close();

      // Leer la respuesta.
      String body = await response.transform(utf8.decoder).join();

      // Devolver la respuesta.
      return body;
    } catch (e) {
      // Manejar el error.
      throw Exception('No se pudo realizar la petición POST: $e');
    } finally {
      // Cerrar el cliente HTTP.
      client.close();
    }
  }
}

// Definir una clase para manejar la lógica de negocio.
class BusinessLogic {
  // Método para obtener los datos de una URL.
  Future<dynamic> getData(String url) async {
    // Crear un cliente HTTP.
    HttpClient client = HttpClient();

    // Realizar la petición GET.
    String body = await client.get(url);

    // Parsear los datos.
    dynamic data = json.decode(body);

    // Devolver los datos.
    return data;
  }

  // Método para enviar los datos a una URL.
  Future<dynamic> sendData(String url, Map<String, dynamic> data) async {
    // Crear un cliente HTTP.
    HttpClient client = HttpClient();

    // Realizar la petición POST.
    String body = await client.post(url, data);

    // Parsear los datos.
    dynamic response = json.decode(body);

    // Devolver los datos.
    return response;
  }
}

// Definir una clase para manejar la interfaz de usuario.
class UserInterface {
  // Método para mostrar los datos en la interfaz de usuario.
  void displayData(dynamic data) {
    // Imprimir los datos en la consola.
    print(data);
  }

  // Método para obtener los datos de la interfaz de usuario.
  Map<String, dynamic> getDataFromUser() {
    // Leer los datos de la consola.
    Map<String, dynamic> data = {};
    print('Ingrese el nombre:');
    data['name'] = stdin.readLineSync();
    print('Ingrese la edad:');
    data['age'] = int.parse(stdin.readLineSync());

    // Devolver los datos.
    return data;
  }
}

// Definir la función principal.
void main() async {
  // Crear una instancia de la clase BusinessLogic.
  BusinessLogic businessLogic = BusinessLogic();

  // Obtener los datos de la URL.
  dynamic data = await businessLogic.getData('https://example.com/data.json');

  // Mostrar los datos en la interfaz de usuario.
  UserInterface userInterface = UserInterface();
  userInterface.displayData(data);

  // Obtener los datos de la interfaz de usuario.
  Map<String, dynamic> dataFromUser = userInterface.getDataFromUser();

  // Enviar los datos a la URL.
  dynamic response = await businessLogic.sendData('https://example.com/send_data.php', dataFromUser);

  // Mostrar la respuesta en la interfaz de usuario.
  userInterface.displayData(response);
}
```

Este código es un ejemplo de una aplicación completa en DART que utiliza una clase HttpClient para realizar peticiones HTTP, una clase BusinessLogic para manejar la lógica de negocio y una clase UserInterface para manejar la interfaz de usuario.

La función principal de la aplicación es obtener los datos de una URL, mostrarlos en la interfaz de usuario, obtener los datos de la interfaz de usuario y enviarlos a una URL.

El código está bien documentado y es fácil de entender. También es modular y reutilizable, lo que facilita la creación de nuevas aplicaciones.