```dart
// Importamos las bibliotecas necesarias.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Creamos una clase para el servidor.
class Servidor {
  // Creamos una variable para el puerto.
  int puerto = 8080;

  // Creamos una variable para el servidor.
  ServerSocket servidor;

  // Creamos un método para iniciar el servidor.
  void iniciarServidor() async {
    // Creamos un servidor en el puerto especificado.
    servidor = await ServerSocket.bind(InternetAddress.anyIPv4, puerto);

    // Creamos un bucle para aceptar conexiones de clientes.
    while (true) {
      // Aceptamos una conexión de un cliente.
      Socket cliente = await servidor.accept();

      // Creamos un manejador para la conexión del cliente.
      ManejadorCliente manejadorCliente = ManejadorCliente(cliente);

      // Iniciamos el manejador del cliente.
      manejadorCliente.iniciarManejador();
    }
  }
}

// Creamos una clase para el manejador del cliente.
class ManejadorCliente {
  // Creamos una variable para el cliente.
  Socket cliente;

  // Creamos una variable para el flujo de entrada.
  Stream<List<int>> flujoEntrada;

  // Creamos una variable para el flujo de salida.
  IOSink flujoSalida;

  // Creamos un método para iniciar el manejador del cliente.
  void iniciarManejador() {
    // Obtenemos el flujo de entrada y el flujo de salida del cliente.
    flujoEntrada = cliente.asBroadcastStream();
    flujoSalida = cliente.sink;

    // Creamos un manejador para los datos recibidos del cliente.
    flujoEntrada.listen((datos) {
      // Decodificamos los datos recibidos del cliente.
      String datosDecodificados = utf8.decode(datos);

      // Escribimos los datos recibidos del cliente en la consola.
      print("Datos recibidos del cliente: $datosDecodificados");

      // Creamos una respuesta para el cliente.
      String respuesta = "Hola, cliente!";

      // Codificamos la respuesta para el cliente.
      List<int> respuestaCodificada = utf8.encode(respuesta);

      // Enviamos la respuesta al cliente.
      flujoSalida.add(respuestaCodificada);
    });
  }
}

// Creamos una función para iniciar el servidor.
void main() {
  // Creamos un servidor.
  Servidor servidor = Servidor();

  // Iniciamos el servidor.
  servidor.iniciarServidor();
}
```

Este código crea un servidor en el puerto 8080 que acepta conexiones de clientes. Cuando un cliente se conecta al servidor, el servidor crea un manejador para el cliente y el manejador del cliente escucha los datos recibidos del cliente. Cuando el cliente envía datos al servidor, el servidor los escribe en la consola y envía una respuesta al cliente.

Este código es complejo porque utiliza muchas características avanzadas de Dart, como la programación asincrónica, los flujos y los sockets. También es complejo porque crea una clase para el servidor y una clase para el manejador del cliente, y define un método para iniciar el servidor y un método para iniciar el manejador del cliente.