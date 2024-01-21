```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {
  // Clase principal del programa
  public static void main(String[] args) {
    // Crear una lista de enteros
    List<Integer> numeros = new ArrayList<>();
    numeros.add(1);
    numeros.add(2);
    numeros.add(3);

    // Crear una lista de cadenas
    List<String> nombres = new ArrayList<>();
    nombres.add("Juan");
    nombres.add("María");
    nombres.add("Pedro");

    // Crear un mapa de cadenas a enteros
    Map<String, Integer> edades = new HashMap<>();
    edades.put("Juan", 20);
    edades.put("María", 25);
    edades.put("Pedro", 30);

    // Iterar sobre la lista de enteros y mostrarlos en consola
    for (int numero : numeros) {
      System.out.println(numero);
    }

    // Iterar sobre la lista de cadenas y mostrarlas en consola
    for (String nombre : nombres) {
      System.out.println(nombre);
    }

    // Iterar sobre el mapa de cadenas a enteros y mostrar las claves y valores en consola
    for (Map.Entry<String, Integer> entrada : edades.entrySet()) {
      System.out.println(entrada.getKey() + " = " + entrada.getValue());
    }

    // Leer un archivo de texto y mostrar su contenido en consola
    try {
      File archivo = new File("archivo.txt");
      Scanner lector = new Scanner(archivo);

      while (lector.hasNextLine()) {
        String linea = lector.nextLine();
        System.out.println(linea);
      }

      lector.close();
    } catch (FileNotFoundException e) {
      System.out.println("El archivo no existe");
    }

    // Escribir en un archivo de texto el contenido de una lista de cadenas
    try {
      File archivo = new File("archivo2.txt");
      PrintWriter escritor = new PrintWriter(archivo);

      for (String nombre : nombres) {
        escritor.println(nombre);
      }

      escritor.close();
    } catch (IOException e) {
      System.out.println("No se pudo escribir en el archivo");
    }
  }
}
```

Explicación del código:

- **Creación de listas y mapas:** Se crean tres listas y un mapa para almacenar datos: una lista de enteros, una lista de cadenas, una lista de cadenas y un mapa de cadenas a enteros.

- **Iteración sobre listas y mapas:** Se itera sobre las listas y el mapa para mostrar sus elementos en consola.

- **Lectura de un archivo de texto:** Se lee un archivo de texto y se muestra su contenido en consola.

- **Escritura en un archivo de texto:** Se escribe en un archivo de texto el contenido de una lista de cadenas.