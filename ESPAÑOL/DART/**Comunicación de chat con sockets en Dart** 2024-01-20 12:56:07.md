```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Crear una clase para representar un usuario
class Usuario {
  final String nombre;
  final String apellido;
  final String correoElectronico;

  Usuario(this.nombre, this.apellido, this.correoElectronico);

  // Definir una función para convertir el usuario a un mapa
  Map<String, dynamic> toJson() => {
        'nombre': nombre,
        'apellido': apellido,
        'correoElectronico': correoElectronico,
      };
}

// Crear una clase para representar un mensaje
class Mensaje {
  final String mensaje;
  final DateTime fechaHora;

  Mensaje(this.mensaje, this.fechaHora);

  // Definir una función para convertir el mensaje a un mapa
  Map<String, dynamic> toJson() => {
        'mensaje': mensaje,
        'fechaHora': fechaHora.toIso8601String(),
      };
}

// Crear una clase para representar un chat
class Chat {
  final String nombre;
  final List<Usuario> usuarios;
  final List<Mensaje> mensajes;

  Chat(this.nombre, this.usuarios, this.mensajes);

  // Definir una función para convertir el chat a un mapa
  Map<String, dynamic> toJson() => {
        'nombre': nombre,
        'usuarios': usuarios.map((usuario) => usuario.toJson()).toList(),
        'mensajes': mensajes.map((mensaje) => mensaje.toJson()).toList(),
      };
}

// Crear una función para enviar un mensaje a un chat
Future<void> enviarMensaje(Chat chat, Mensaje mensaje) async {
  // Abrir una conexión al servidor
  var socket = await Socket.connect('127.0.0.1', 8080);

  // Convertir el mensaje a un mapa
  var mensajeJson = json.encode(mensaje.toJson());

  // Enviar el mensaje al servidor
  socket.write(mensajeJson);

  // Cerrar la conexión
  socket.close();
}

// Crear una función para recibir mensajes de un chat
Stream<Mensaje> recibirMensajes(Chat chat) async* {
  // Abrir una conexión al servidor
  var socket = await Socket.connect('127.0.0.1', 8080);

  // Enviar el nombre del chat al servidor
  socket.write(chat.nombre);

  // Recibir mensajes del servidor
  await for (var mensajeJson in socket) {
    // Convertir el mensaje a un objeto
    var mensaje = Mensaje.fromJson(json.decode(mensajeJson));

    // Emitir el mensaje
    yield mensaje;
  }

  // Cerrar la conexión
  socket.close();
}

// Crear un objeto usuario
var usuario1 = Usuario('Juan', 'Pérez', 'juan.perez@ejemplo.com');

// Crear un objeto mensaje
var mensaje1 = Mensaje('Hola a todos!', DateTime.now());

// Crear un objeto chat
var chat1 = Chat('Chat 1', [usuario1], [mensaje1]);

// Enviar un mensaje al chat
enviarMensaje(chat1, mensaje1);

// Recibir mensajes del chat
recibirMensajes(chat1).listen((mensaje) {
  print('Recibido mensaje: ${mensaje.mensaje}');
});
```

Este código crea una clase para representar un usuario, una clase para representar un mensaje y una clase para representar un chat. También define una función para enviar un mensaje a un chat y una función para recibir mensajes de un chat.

El código crea un objeto usuario, un objeto mensaje y un objeto chat, y luego envía el mensaje al chat. A continuación, utiliza la función `recibirMensajes()` para escuchar los mensajes del chat y los imprime en la consola.

Este código es complejo porque utiliza varias clases, funciones y flujos de datos. Sin embargo, es un ejemplo útil de cómo se pueden utilizar las clases, las funciones y los flujos de datos en Dart para crear aplicaciones complejas.