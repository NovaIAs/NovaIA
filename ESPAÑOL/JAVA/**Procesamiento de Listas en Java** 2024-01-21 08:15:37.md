```java
import java.util.*;
import java.io.*;

public class Complejo {

    public static void main(String[] args) {
        // Definicion de variables
        int[] numeros = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        String[] palabras = {"Hola", "Mundo", "Java", "Complejo", "Codigo"};
        List<Integer> listaNumeros = new ArrayList<>();
        List<String> listaPalabras = new ArrayList<>();

        // Llenar las listas con los arrays
        for (int numero : numeros) {
            listaNumeros.add(numero);
        }
        for (String palabra : palabras) {
            listaPalabras.add(palabra);
        }

        // Imprimir las listas
        System.out.println("Lista de números:");
        for (int numero : listaNumeros) {
            System.out.println(numero);
        }
        System.out.println();

        System.out.println("Lista de palabras:");
        for (String palabra : listaPalabras) {
            System.out.println(palabra);
        }
        System.out.println();

        // Ordenar las listas
        Collections.sort(listaNumeros);
        Collections.sort(listaPalabras);

        // Imprimir las listas ordenadas
        System.out.println("Lista de números ordenada:");
        for (int numero : listaNumeros) {
            System.out.println(numero);
        }
        System.out.println();

        System.out.println("Lista de palabras ordenada:");
        for (String palabra : listaPalabras) {
            System.out.println(palabra);
        }
        System.out.println();

        // Buscar un elemento en las listas
        Scanner scanner = new Scanner(System.in);

        System.out.println("Ingrese un número a buscar:");
        int numeroABuscar = scanner.nextInt();
        int indiceNumero = Collections.binarySearch(listaNumeros, numeroABuscar);

        if (indiceNumero >= 0) {
            System.out.println("El número " + numeroABuscar + " se encuentra en la posición " + indiceNumero);
        } else {
            System.out.println("El número " + numeroABuscar + " no se encuentra en la lista");
        }

        System.out.println();

        System.out.println("Ingrese una palabra a buscar:");
        String palabraABuscar = scanner.next();
        int indicePalabra = Collections.binarySearch(listaPalabras, palabraABuscar);

        if (indicePalabra >= 0) {
            System.out.println("La palabra " + palabraABuscar + " se encuentra en la posición " + indicePalabra);
        } else {
            System.out.println("La palabra " + palabraABuscar + " no se encuentra en la lista");
        }

        scanner.close();
    }
}
```

Este código es un poco más complejo que los anteriores y realiza varias tareas diferentes.

En primer lugar, define dos arrays, uno de números enteros y otro de cadenas de caracteres. A continuación, crea dos listas, una para cada array, y las llena con los elementos de los arrays.

Luego, ordena las listas usando el método `sort()` de la clase `Collections`. Este método ordena los elementos de la lista de forma ascendente.

Después, pide al usuario que introduzca un número y una cadena de caracteres a buscar. A continuación, busca los elementos introducidos en las listas usando el método `binarySearch()` de la clase `Collections`. Este método devuelve el índice del elemento buscado en la lista, o un valor negativo si el elemento no se encuentra en la lista.

Finalmente, imprime los resultados de la búsqueda.

Este código es un ejemplo de cómo se pueden usar las listas en Java para almacenar y procesar datos de forma eficiente.