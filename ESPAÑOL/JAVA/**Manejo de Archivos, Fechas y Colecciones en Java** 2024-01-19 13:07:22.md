```java
// Importación de bibliotecas
import java.util.*;
import java.io.*;
import java.text.*;

// Clase principal
public class CodigoComplejo {

    // Método principal
    public static void main(String[] args) {

        // Creación de una lista de números enteros
        List<Integer> numeros = new ArrayList<>();
        numeros.add(1);
        numeros.add(2);
        numeros.add(3);
        numeros.add(4);
        numeros.add(5);

        // Creación de un mapa de clave-valor
        Map<String, String> mapa = new HashMap<>();
        mapa.put("nombre", "Juan");
        mapa.put("apellido", "García");
        mapa.put("edad", "25");

        // Creación de un objeto Date
        Date fecha = new Date();

        // Formateo de la fecha
        SimpleDateFormat formatoFecha = new SimpleDateFormat("dd/MM/yyyy");
        String fechaFormateada = formatoFecha.format(fecha);

        // Escritura de la fecha formateada en un archivo de texto
        try {
            FileWriter escritor = new FileWriter("fecha.txt");
            escritor.write(fechaFormateada);
            escritor.close();
        } catch (IOException e) {
            System.out.println("Error al escribir el archivo.");
            e.printStackTrace();
        }

        // Lectura del archivo de texto y almacenamiento de su contenido en una cadena
        String contenido = "";
        try {
            FileReader lector = new FileReader("fecha.txt");
            BufferedReader lectorBuffer = new BufferedReader(lector);
            String linea;
            while ((linea = lectorBuffer.readLine()) != null) {
                contenido += linea;
            }
            lectorBuffer.close();
            lector.close();
        } catch (IOException e) {
            System.out.println("Error al leer el archivo.");
            e.printStackTrace();
        }

        // Impresión de la cadena en consola
        System.out.println(contenido);

        // Búsqueda de un elemento en la lista de números enteros
        int elementoBuscado = 3;
        int indice = numeros.indexOf(elementoBuscado);
        if (indice != -1) {
            System.out.println("El elemento " + elementoBuscado + " se encuentra en la posición " + indice);
        } else {
            System.out.println("El elemento " + elementoBuscado + " no se encuentra en la lista.");
        }

        // Obtención del valor de una clave en el mapa
        String valorBuscado = "nombre";
        String valor = mapa.get(valorBuscado);
        if (valor != null) {
            System.out.println("El valor de la clave " + valorBuscado + " es " + valor);
        } else {
            System.out.println("La clave " + valorBuscado + " no existe en el mapa.");
        }
    }
}
```

Explicación del código:

1. Se importan las bibliotecas necesarias para el funcionamiento del código.
2. Se crea una lista de números enteros y se añaden algunos elementos.
3. Se crea un mapa de clave-valor y se añaden algunos pares clave-valor.
4. Se crea un objeto Date para obtener la fecha actual.
5. Se formatea la fecha utilizando un objeto SimpleDateFormat y se almacena en una cadena.
6. Se escribe la fecha formateada en un archivo de texto utilizando un objeto FileWriter.
7. Se lee el contenido del archivo de texto y se almacena en una cadena utilizando un objeto FileReader y un objeto BufferedReader.
8. Se imprime la cadena en consola.
9. Se busca un elemento en la lista de números enteros utilizando el método indexOf() y se imprime el resultado en consola.
10. Se obtiene el valor de una clave en el mapa utilizando el método get() y se imprime el resultado en consola.