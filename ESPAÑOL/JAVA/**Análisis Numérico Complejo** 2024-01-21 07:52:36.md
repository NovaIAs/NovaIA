```java
// Importación de las librerías necesarias
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

// Clase principal del programa
public class CodigoComplejo {

    // Método principal del programa
    public static void main(String[] args) {
        // Creación del objeto Scanner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Creación de una lista de enteros para almacenar los números ingresados por el usuario
        ArrayList<Integer> listaNumeros = new ArrayList<Integer>();

        // Creación de un mapa para almacenar los números ingresados por el usuario junto con su frecuencia
        Map<Integer, Integer> mapaFrecuencias = new HashMap<Integer, Integer>();

        // Ciclo para leer los números ingresados por el usuario y almacenarlos en la lista
        System.out.println("Ingrese los números separados por comas:");
        String linea = scanner.nextLine();
        String[] numeros = linea.split(",");
        for (String numero : numeros) {
            listaNumeros.add(Integer.parseInt(numero));
        }

        // Ciclo para calcular la frecuencia de cada número ingresado por el usuario y almacenarla en el mapa
        for (Integer numero : listaNumeros) {
            if (mapaFrecuencias.containsKey(numero)) {
                mapaFrecuencias.put(numero, mapaFrecuencias.get(numero) + 1);
            } else {
                mapaFrecuencias.put(numero, 1);
            }
        }

        // Ordenación de la lista de números en orden ascendente
        Collections.sort(listaNumeros);

        // Impresión de la lista de números ordenados
        System.out.println("Lista de números ordenados:");
        for (Integer numero : listaNumeros) {
            System.out.print(numero + " ");
        }

        // Impresión del mapa de frecuencias
        System.out.println();
        System.out.println("Mapa de frecuencias:");
        for (Map.Entry<Integer, Integer> entrada : mapaFrecuencias.entrySet()) {
            System.out.println(entrada.getKey() + " -> " + entrada.getValue());
        }
    }
}
```

Explicación del código:

* La clase `CodigoComplejo` contiene el método `main`, que es el punto de entrada del programa.
* El método `main` crea un objeto `Scanner` para leer la entrada del usuario.
* El método `main` crea una lista de enteros `listaNumeros` para almacenar los números ingresados por el usuario.
* El método `main` crea un mapa `mapaFrecuencias` para almacenar los números ingresados por el usuario junto con su frecuencia.
* El método `main` lee los números ingresados por el usuario y los almacena en la lista `listaNumeros`.
* El método `main` calcula la frecuencia de cada número ingresado por el usuario y lo almacena en el mapa `mapaFrecuencias`.
* El método `main` ordena la lista `listaNumeros` en orden ascendente.
* El método `main` imprime la lista `listaNumeros` ordenada.
* El método `main` imprime el mapa `mapaFrecuencias`.