```dart
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Función para manejar las solicitudes HTTP.
Future<void> handleRequest(HttpRequest request) async {
  // Obtenemos el tipo de solicitud.
  String method = request.method;

  // Obtenemos la ruta de la solicitud.
  String uri = request.uri.path;

  // Obtenemos el cuerpo de la solicitud.
  String body = await request.transform(utf8.decoder).join();

  // Procesamos la solicitud según el tipo y la ruta.
  switch (method) {
    case 'GET':
      // Si es una solicitud GET, obtenemos el recurso solicitado.
      switch (uri) {
        case '/usuarios':
          // Si la ruta es '/usuarios', obtenemos todos los usuarios.
          List<Usuario> usuarios = await obtenerUsuarios();
          request.response
            ..statusCode = 200
            ..write(json.encode(usuarios));
          break;
        case '/productos':
          // Si la ruta es '/productos', obtenemos todos los productos.
          List<Producto> productos = await obtenerProductos();
          request.response
            ..statusCode = 200
            ..write(json.encode(productos));
          break;
        default:
          // Si la ruta no existe, enviamos un código de error 404.
          request.response.statusCode = 404;
          break;
      }
      break;
    case 'POST':
      // Si es una solicitud POST, creamos un nuevo recurso.
      switch (uri) {
        case '/usuarios':
          // Si la ruta es '/usuarios', creamos un nuevo usuario.
          Usuario usuario = json.decode(body);
          await crearUsuario(usuario);
          request.response.statusCode = 201;
          break;
        case '/productos':
          // Si la ruta es '/productos', creamos un nuevo producto.
          Producto producto = json.decode(body);
          await crearProducto(producto);
          request.response.statusCode = 201;
          break;
        default:
          // Si la ruta no existe, enviamos un código de error 404.
          request.response.statusCode = 404;
          break;
      }
      break;
    case 'PUT':
      // Si es una solicitud PUT, actualizamos un recurso existente.
      switch (uri) {
        case '/usuarios':
          // Si la ruta es '/usuarios', actualizamos un usuario.
          Usuario usuario = json.decode(body);
          await actualizarUsuario(usuario);
          request.response.statusCode = 200;
          break;
        case '/productos':
          // Si la ruta es '/productos', actualizamos un producto.
          Producto producto = json.decode(body);
          await actualizarProducto(producto);
          request.response.statusCode = 200;
          break;
        default:
          // Si la ruta no existe, enviamos un código de error 404.
          request.response.statusCode = 404;
          break;
      }
      break;
    case 'DELETE':
      // Si es una solicitud DELETE, eliminamos un recurso existente.
      switch (uri) {
        case '/usuarios':
          // Si la ruta es '/usuarios', eliminamos un usuario.
          Usuario usuario = json.decode(body);
          await eliminarUsuario(usuario);
          request.response.statusCode = 200;
          break;
        case '/productos':
          // Si la ruta es '/productos', eliminamos un producto.
          Producto producto = json.decode(body);
          await eliminarProducto(producto