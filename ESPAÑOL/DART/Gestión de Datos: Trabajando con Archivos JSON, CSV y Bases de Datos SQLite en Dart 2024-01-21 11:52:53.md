```dart
// Importamos las bibliotecas necesarias
import 'dart:async';
import 'dart:convert';
import 'dart:io';

// Creamos una clase con los métodos necesarios para trabajar con archivos JSON
class ManejadorJSON {
  // Método para leer un archivo JSON y devolver su contenido como un mapa
  Future<Map<String, dynamic>> leerJSON(String rutaArchivo) async {
    // Intentamos leer el archivo
    try {
      // Leemos el archivo y lo convertimos en una cadena
      String contenido = await File(rutaArchivo).readAsString();

      // Convertimos la cadena en un mapa
      Map<String, dynamic> mapa = json.decode(contenido);

      // Devolvemos el mapa
      return mapa;
    }

    // Si no se puede leer el archivo, lanzamos una excepción
    catch (e) {
      throw Exception('No se pudo leer el archivo JSON: $e');
    }
  }

  // Método para escribir un mapa en un archivo JSON
  Future<void> escribirJSON(String rutaArchivo, Map<String, dynamic> mapa) async {
    // Convertimos el mapa en una cadena
    String contenido = json.encode(mapa);

    // Escribimos la cadena en el archivo
    await File(rutaArchivo).writeAsString(contenido);
  }
}

// Creamos una clase con los métodos necesarios para trabajar con archivos CSV
class ManejadorCSV {
  // Método para leer un archivo CSV y devolver su contenido como una lista de listas
  Future<List<List<String>>> leerCSV(String rutaArchivo) async {
    // Intentamos leer el archivo
    try {
      // Leemos el archivo y lo convertimos en una cadena
      String contenido = await File(rutaArchivo).readAsString();

      // Dividimos la cadena en líneas
      List<String> lineas = contenido.split('\n');

      // Dividimos cada línea en columnas
      List<List<String>> columnas = [];
      for (String linea in lineas) {
        List<String> columna = linea.split(',');
        columnas.add(columna);
      }

      // Devolvemos la lista de listas
      return columnas;
    }

    // Si no se puede leer el archivo, lanzamos una excepción
    catch (e) {
      throw Exception('No se pudo leer el archivo CSV: $e');
    }
  }

  // Método para escribir una lista de listas en un archivo CSV
  Future<void> escribirCSV(String rutaArchivo, List<List<String>> columnas) async {
    // Convertimos la lista de listas en una cadena
    String contenido = '';
    for (List<String> columna in columnas) {
      contenido += columna.join(',') + '\n';
    }

    // Escribimos la cadena en el archivo
    await File(rutaArchivo).writeAsString(contenido);
  }
}

// Creamos una clase con los métodos necesarios para trabajar con bases de datos SQLite
class ManejadorSQLite {
  // Método para crear una base de datos SQLite y devolver una conexión a la misma
  Future<Database> crearBaseDeDatos(String nombreBaseDeDatos) async {
    // Intentamos crear la base de datos
    try {
      // Creamos la base de datos
      Database database = await openDatabase(nombreBaseDeDatos);

      // Devolvemos la conexión a la base de datos
      return database;
    }

    // Si no se puede crear la base de datos, lanzamos una excepción
    catch (e) {
      throw Exception('No se pudo crear la base de datos SQLite: $e');
    }
  }

  // Método para cerrar una conexión a una base de datos SQLite
  Future<void> cerrarBaseDeDatos(Database database) async {
    // Intentamos cerrar la conexión
    try {
      // Cerramos la conexión
      await database.close();
    }

    // Si no se puede cerrar la conexión, lanzamos una excepción
    catch (e) {
      throw Exception('No se pudo cerrar la conexión a la base de datos SQLite: $e');
    }
  }

  // Método para ejecutar una consulta SQL en una base de datos SQLite
  Future<List<Map<String, dynamic>>> ejecutarConsulta(Database database, String consulta) async {
    // Intentamos ejecutar la consulta
    try {
      // Ejecutamos la consulta
      List<Map<String, dynamic>> resultados = await database.rawQuery(consulta);

      // Devolvemos los resultados de la consulta
      return resultados;
    }

    // Si no se puede ejecutar la consulta, lanzamos una excepción
    catch (e) {
      throw Exception('No se pudo ejecutar la consulta SQL en la base de datos SQLite: $e');
    }
  }
}

// Creamos una función principal para probar las clases creadas
void main() async {
  // Creamos un manejador JSON
  ManejadorJSON manejadorJSON = ManejadorJSON();

  // Creamos un manejador CSV
  ManejadorCSV manejadorCSV = ManejadorCSV();

  // Creamos un manejador SQLite
  ManejadorSQLite manejadorSQLite = ManejadorSQLite();

  // Creamos una base de datos SQLite
  Database database = await manejadorSQLite.crearBaseDeDatos('mi_base_de_datos.db');

  // Ejecutamos una consulta SQL en la base de datos SQLite
  List<Map<String, dynamic>> resultados = await manejadorSQLite.ejecutarConsulta(database, 'SELECT * FROM mi_tabla');

  // Imprimimos los resultados de la consulta
  print(resultados);

  // Cerramos la conexión a la base de datos SQLite
  await manejadorSQLite.cerrarBaseDeDatos(database);

  // Leemos un archivo JSON
  Map<String, dynamic> mapa = await manejadorJSON.leerJSON('mi_archivo.json');

  // Imprimimos el contenido del archivo JSON
  print(mapa);

  // Escribimos un archivo JSON
  await manejadorJSON.escribirJSON('mi_archivo.json', mapa);

  // Leemos un archivo CSV
  List<List<String>> columnas = await manejadorCSV.leerCSV('mi_archivo.csv');

  // Imprimimos el contenido del archivo CSV
  print(columnas);

  // Escribimos un archivo CSV
  await manejadorCSV.escribirCSV('mi_archivo.csv', columnas);
}
```