```dart
// Importamos el paquete 'csv' para leer archivos CSV.
import 'package:csv/csv.dart';

// Definimos una función para leer un archivo CSV y devolver una lista de listas.
List<List<String>> leerCSV(String rutaArchivo) {
  // Abrimos el archivo CSV en modo de lectura.
  var archivo = File(rutaArchivo).openRead();

  // Leemos el archivo CSV usando el paquete 'csv'.
  var datos = const CsvToListConverter().convert(archivo);

  // Cerramos el archivo CSV.
  archivo.close();

  // Devolvemos los datos en forma de una lista de listas.
  return datos;
}

// Definimos una función para escribir una lista de listas en un archivo CSV.
void escribirCSV(String rutaArchivo, List<List<String>> datos) {
  // Abrimos el archivo CSV en modo de escritura.
  var archivo = File(rutaArchivo).openWrite();

  // Escribimos los datos en el archivo CSV usando el paquete 'csv'.
  var datosCSV = const ListToCsvConverter().convert(datos);
  archivo.write(datosCSV);

  // Cerramos el archivo CSV.
  archivo.close();
}

// Definimos una función principal.
main() {
  // Leemos los datos del archivo CSV 'datos.csv'.
  var datos = leerCSV('datos.csv');

  // Modificamos los datos agregando una columna con el total de cada fila.
  for (var i = 0; i < datos.length; i++) {
    var fila = datos[i];
    var total = 0;
    for (var j = 1; j < fila.length; j++) {
      total += int.parse(fila[j]);
    }
    fila.add(total.toString());
  }

  // Escribimos los datos modificados en el archivo CSV 'datos_modificados.csv'.
  escribirCSV('datos_modificados.csv', datos);

  // Imprimimos los datos modificados en la consola.
  for (var fila in datos) {
    print(fila);
  }
}
```

Explicación del código:

1. Importamos el paquete 'csv' que nos permitirá leer y escribir archivos CSV.

2. Definimos una función `leerCSV()` que recibe la ruta de un archivo CSV y devuelve una lista de listas. Esta función utiliza el paquete 'csv' para leer el archivo CSV y devolver los datos en forma de una lista de listas.

3. Definimos una función `escribirCSV()` que recibe la ruta de un archivo CSV y una lista de listas. Esta función utiliza el paquete 'csv' para escribir los datos en el archivo CSV.

4. En la función `main()`, leemos los datos del archivo CSV 'datos.csv' utilizando la función `leerCSV()`.

5. Modificamos los datos agregando una columna con el total de cada fila.

6. Escribimos los datos modificados en el archivo CSV 'datos_modificados.csv' utilizando la función `escribirCSV()`.

7. Imprimimos los datos modificados en la consola.