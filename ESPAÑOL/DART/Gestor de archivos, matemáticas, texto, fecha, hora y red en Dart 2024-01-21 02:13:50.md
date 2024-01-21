```dart
// Importamos las librerías necesarias
import 'dart:async';
import 'dart:io';

// Definimos una clase para gestionar los archivos
class FileManager {

  // Constructor
  FileManager() {
    print('Se ha creado una instancia de FileManager');
  }

  // Método para crear un archivo
  Future<File> crearArchivo(String ruta, String contenido) async {
    File file = File(ruta);
    await file.writeAsString(contenido);
    print('Se ha creado el archivo $ruta');
    return file;
  }

  // Método para leer un archivo
  Future<String> leerArchivo(String ruta) async {
    File file = File(ruta);
    String contenido = await file.readAsString();
    print('Se ha leído el archivo $ruta');
    return contenido;
  }

  // Método para eliminar un archivo
  Future<bool> eliminarArchivo(String ruta) async {
    File file = File(ruta);
    bool eliminado = await file.delete();
    print('Se ha eliminado el archivo $ruta');
    return eliminado;
  }
}

// Definimos una clase para gestionar las operaciones matemáticas
class MathManager {

  // Constructor
  MathManager() {
    print('Se ha creado una instancia de MathManager');
  }

  // Método para sumar dos números
  int sumar(int a, int b) {
    int resultado = a + b;
    print('Se ha sumado $a y $b, el resultado es $resultado');
    return resultado;
  }

  // Método para restar dos números
  int restar(int a, int b) {
    int resultado = a - b;
    print('Se ha restado $a y $b, el resultado es $resultado');
    return resultado;
  }

  // Método para multiplicar dos números
  int multiplicar(int a, int b) {
    int resultado = a * b;
    print('Se ha multiplicado $a y $b, el resultado es $resultado');
    return resultado;
  }

  // Método para dividir dos números
  double dividir(int a, int b) {
    double resultado = a / b;
    print('Se ha dividido $a y $b, el resultado es $resultado');
    return resultado;
  }
}

// Definimos una clase para gestionar las operaciones de texto
class TextManager {

  // Constructor
  TextManager() {
    print('Se ha creado una instancia de TextManager');
  }

  // Método para contar el número de palabras en una cadena de texto
  int contarPalabras(String texto) {
    int palabras = texto.split(' ').length;
    print('El texto tiene $palabras palabras');
    return palabras;
  }

  // Método para contar el número de caracteres en una cadena de texto
  int contarCaracteres(String texto) {
    int caracteres = texto.length;
    print('El texto tiene $caracteres caracteres');
    return caracteres;
  }

  // Método para convertir una cadena de texto a mayúsculas
  String aMayúsculas(String texto) {
    String textoMayúsculas = texto.toUpperCase();
    print('El texto en mayúsculas es $textoMayúsculas');
    return textoMayúsculas;
  }

  // Método para convertir una cadena de texto a minúsculas
  String aMinúsculas(String texto) {
    String textoMinúsculas = texto.toLowerCase();
    print('El texto en minúsculas es $textoMinúsculas');
    return textoMinúsculas;
  }
}

// Definimos una clase para gestionar las operaciones de fecha y hora
class DateManager {

  // Constructor
  DateManager() {
    print('Se ha creado una instancia de DateManager');
  }

  // Método para obtener la fecha actual
  DateTime obtenerFechaActual() {
    DateTime fechaActual = DateTime.now();
    print('La fecha actual es $fechaActual');
    return fechaActual;
  }

  // Método para obtener la hora actual
  TimeOfDay obtenerHoraActual() {
    TimeOfDay horaActual = TimeOfDay.now();
    print('La hora actual es $horaActual');
    return horaActual;
  }

  // Método para sumar días a una fecha
  DateTime sumarDías(DateTime fecha, int días) {
    DateTime fechaNueva = fecha.add(Duration(days: días));
    print('Se han sumado $días días a la fecha $fecha, la nueva fecha es $fechaNueva');
    return fechaNueva;
  }

  // Método para restar días a una fecha
  DateTime restarDías(DateTime fecha, int días) {
    DateTime fechaNueva = fecha.subtract(Duration(days: días));
    print('Se han restado $días días a la fecha $fecha, la nueva fecha es $fechaNueva');
    return fechaNueva;
  }
}

// Definimos una clase para gestionar las operaciones de redes
class NetworkManager {

  // Constructor
  NetworkManager() {
    print('Se ha creado una instancia de NetworkManager');
  }

  // Método para hacer una petición GET a una URL
  Future<String> hacerPeticiónGET(String url) async {
    HttpClient client = HttpClient();
    client.connectionTimeout = Duration(seconds: 10);
    HttpClientRequest request = await client.getUrl(Uri.parse(url));
    HttpClientResponse response = await request.close();
    String contenido = await response.transform(utf8.decoder).join();
    print('Se ha realizado una petición GET a la URL $url, el contenido es $contenido');
    return contenido;
  }

  // Método para hacer una petición POST a una URL
  Future<String> hacerPeticiónPOST(String url, String contenido) async {
    HttpClient client = HttpClient();
    client.connectionTimeout = Duration(seconds: 10);
    HttpClientRequest request = await client.postUrl(Uri.parse(url));
    request.headers.add('Content-Type', 'application/json');
    request.write(contenido);
    HttpClientResponse response = await request.close();
    String contenidoRespuesta = await response.transform(utf8.decoder).join();
    print('Se ha realizado una petición POST a la URL $url, el contenido enviado es $contenido, el contenido de la respuesta es $contenidoRespuesta');
    return contenidoRespuesta;
  }
}

// Definimos una clase principal
class Main {

  // Función principal
  static void main(List<String> args) {

    // Creamos una instancia de FileManager
    FileManager fileManager = FileManager();

    // Creamos un archivo
    fileManager.crearArchivo('prueba.txt', 'Hola mundo');

    // Leemos el archivo
    String contenido = fileManager.leerArchivo('prueba.txt');

    // Eliminamos el archivo
    fileManager.eliminarArchivo('prueba.txt');

    // Creamos una instancia de MathManager
    MathManager mathManager = MathManager();

    // Sumamos dos números
    int resultadoSuma = mathManager.sumar(1, 2);

    // Restamos dos números
    int resultadoResta = mathManager.restar(3, 2);

    // Multiplicamos dos números
    int resultadoMultiplicación = mathManager.multiplicar(4, 5);

    // Dividimos dos números
    double resultadoDivisión = mathManager.dividir(10, 2);

    // Creamos una instancia de TextManager
    TextManager textManager = TextManager();

    // Contamos el número de palabras en un texto
    int palabras = textManager.contarPalabras('Hola mundo');

    // Contamos el número de caracteres en un texto
    int caracteres = textManager.contarCaracteres('Hola mundo');

    // Convertimos un texto a mayúsculas
    String textoMayúsculas = textManager.aMayúsculas('Hola mundo');

    // Convertimos un texto a minúsculas
    String textoMinúsculas = textManager.aMinúsculas('HOLA MUNDO');

    // Creamos una instancia de DateManager
    DateManager dateManager = DateManager();

    // Obtenemos la fecha actual
    DateTime fechaActual = dateManager.obtenerFechaActual();

    // Obtenemos la hora actual
    TimeOfDay horaActual = dateManager.obtenerHoraActual();

    // Sumamos días a una fecha
    DateTime fechaNueva = dateManager.sumarDías(fechaActual, 10);

    // Restamos días a una fecha
    DateTime fechaAnterior = dateManager.restarDías(fechaActual, 20);

    // Creamos una instancia de NetworkManager
    NetworkManager networkManager = NetworkManager();

    // Hacemos una petición GET a una URL
    String contenidoGET = networkManager.hacerPeticiónGET('https://google.com');

    // Hacemos una petición POST a una URL
    String contenidoPOST = networkManager.hacerPeticiónPOST('https://google.com', '{"nombre": "Juan", "apellido": "Pérez"}');

    // Imprimimos los resultados
    print('Resultado de la suma: $resultadoSuma');
    print('Resultado de la resta: $resultadoResta');
    print('Resultado de la multiplicación: $resultadoMultiplicación');
    print('Resultado de la división: $resultadoDivisión');
    print('Número de palabras: $palabras');
    print('Número de caracteres: $caracteres');
    print('Texto en mayúsculas: $textoMayúsculas');
    print('Texto en minúsculas: $textoMinúsculas');
    print('Fecha actual: $fechaActual');
    print('Hora actual: $horaActual');
    print('Fecha nueva: $fechaNueva');
    print('Fecha anterior: $fechaAnterior');
    print('Contenido de la petición GET: $contenidoGET');
    print('Contenido de la petición POST: $contenidoPOST');
  }
}

// Explicación del código:

// El código define cinco clases: FileManager, MathManager, TextManager, DateManager y NetworkManager. Cada clase gestiona un tipo específico de operaciones.

// La clase FileManager gestiona las operaciones de archivos, como crear, leer y eliminar archivos.

// La clase MathManager gestiona las operaciones matemáticas, como sumar, restar, multiplicar y dividir.

// La clase TextManager gestiona las operaciones de texto, como contar el número de palabras y caracteres, convertir texto a mayúsculas y minúsculas, etc.

// La clase DateManager gestiona las operaciones de fecha y hora, como obtener la fecha y hora actuales, sumar y restar días a una fecha, etc.

// La clase NetworkManager gestiona las operaciones de redes, como hacer peticiones GET y POST a una URL.

// La clase Main es la clase principal del programa. Define la función main, que es el punto de entrada del programa.

// En la función main, se crean instancias de las cinco clases y se llaman a sus métodos para realizar diversas operaciones.

// Los resultados de las operaciones se imprimen en la consola.