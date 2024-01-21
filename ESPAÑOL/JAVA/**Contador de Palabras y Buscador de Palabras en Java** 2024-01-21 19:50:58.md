```java
import java.util.*;
import java.util.regex.*;
import java.io.*;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Crear un objeto Scanner para leer desde la consola
        Scanner scanner = new Scanner(System.in);

        // Crear una lista de palabras vacía
        List<String> palabras = new ArrayList<>();

        // Leer las palabras de la consola y añadirlas a la lista
        System.out.println("Escribe una frase o texto:");
        String frase = scanner.nextLine();

        // Dividir la frase en palabras
        String[] palabrasArray = frase.split(" ");

        // Añadir las palabras a la lista
        for (String palabra : palabrasArray) {
            palabras.add(palabra);
        }

        // Crear un mapa para contar las apariciones de cada palabra
        Map<String, Integer> contadorPalabras = new HashMap<>();

        // Contar las apariciones de cada palabra
        for (String palabra : palabras) {
            if (contadorPalabras.containsKey(palabra)) {
                contadorPalabras.put(palabra, contadorPalabras.get(palabra) + 1);
            } else {
                contadorPalabras.put(palabra, 1);
            }
        }

        // Mostrar las palabras y sus apariciones
        for (Map.Entry<String, Integer> entrada : contadorPalabras.entrySet()) {
            System.out.println(entrada.getKey() + ": " + entrada.getValue());
        }

        // Crear un objeto Pattern para buscar palabras que empiecen por "a"
        Pattern patron = Pattern.compile("^a.*$");

        // Buscar las palabras que empiecen por "a"
        List<String> palabrasConA = new ArrayList<>();
        for (String palabra : palabras) {
            if (patron.matcher(palabra).matches()) {
                palabrasConA.add(palabra);
            }
        }

        // Mostrar las palabras que empiezan por "a"
        System.out.println("Palabras que empiezan por \"a\":");
        for (String palabra : palabrasConA) {
            System.out.println(palabra);
        }

        // Crear un objeto File para escribir en un archivo
        File file = new File("palabras.txt");

        // Crear un objeto PrintWriter para escribir en el archivo
        PrintWriter writer = new PrintWriter(file);

        // Escribir las palabras en el archivo
        for (String palabra : palabras) {
            writer.println(palabra);
        }

        // Cerrar el objeto PrintWriter
        writer.close();

        // Leer las palabras del archivo y mostrarlas
        System.out.println("Palabras leídas del archivo:");
        try {
            Scanner scannerArchivo = new Scanner(file);
            while (scannerArchivo.hasNextLine()) {
                String palabra = scannerArchivo.nextLine();
                System.out.println(palabra);
            }
            scannerArchivo.close();
        } catch (FileNotFoundException e) {
            System.out.println("No se pudo encontrar el archivo.");
        }
    }
}
```

Explicación del código:

* El código utiliza la clase `Scanner` para leer las palabras de la consola.
* El código divide la frase en palabras utilizando el método `split()`.
* El código utiliza la clase `HashMap` para contar las apariciones de cada palabra.
* El código utiliza la clase `Pattern` para buscar palabras que empiecen por "a".
* El código utiliza la clase `File` para escribir las palabras en un archivo.
* El código utiliza la clase `PrintWriter` para escribir las palabras en el archivo.
* El código utiliza la clase `Scanner` para leer las palabras del archivo.