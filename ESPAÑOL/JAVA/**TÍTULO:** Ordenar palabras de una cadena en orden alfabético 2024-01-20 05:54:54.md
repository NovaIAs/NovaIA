**Problema:**
Dada una cadena de texto, devolver una nueva cadena con todas las palabras ordenadas alfabéticamente.

**Código:**

```java
import java.util.*;

public class OrdenarPalabras {

    public static void main(String[] args) {
        String cadena = "Hola mundo, cómo estás?";

        // Dividir la cadena en palabras
        String[] palabras = cadena.split(" ");

        // Ordenar las palabras alfabéticamente
        Arrays.sort(palabras);

        // Concatenar las palabras en una nueva cadena
        String cadenaOrdenada = "";
        for (String palabra : palabras) {
            cadenaOrdenada += palabra + " ";
        }

        // Eliminar el espacio en blanco final
        cadenaOrdenada = cadenaOrdenada.trim();

        // Imprimir la cadena ordenada
        System.out.println(cadenaOrdenada);
    }
}
```

**Explicación:**

* La primera línea del código importa la biblioteca `java.util`, que contiene varias clases y métodos útiles para trabajar con datos.
* La segunda línea define la clase `OrdenarPalabras` que contiene el método `main`.
* El método `main` toma un array de cadenas de texto como argumento y es el punto de entrada del programa.
* La variable `cadena` contiene la cadena de texto que queremos ordenar.
* La línea `String[] palabras = cadena.split(" ");` divide la cadena en palabras usando el espacio en blanco como delimitador.
* La línea `Arrays.sort(palabras);` ordena las palabras alfabéticamente.
* El bucle `for` recorre las palabras ordenadas y las concatena en una nueva cadena.
* La línea `cadenaOrdenada = cadenaOrdenada.trim();` elimina el espacio en blanco final de la cadena ordenada.
* La línea `System.out.println(cadenaOrdenada);` imprime la cadena ordenada en la consola.