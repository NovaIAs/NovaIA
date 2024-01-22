```java
// Importación de las bibliotecas necesarias
import java.util.Scanner;
import java.util.Random;
import java.util.ArrayList;
import java.util.List;

// Clase principal del programa
public class Complejo {

    // Método principal del programa
    public static void main(String[] args) {
        // Creación de un objeto Scanner para leer datos de la consola
        Scanner scanner = new Scanner(System.in);

        // Creación de un objeto Random para generar números aleatorios
        Random random = new Random();

        // Creación de una lista de números aleatorios
        List<Integer> numerosAleatorios = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            numerosAleatorios.add(random.nextInt(100));
        }

        // Muestra de la lista de números aleatorios
        System.out.println("Lista de números aleatorios:");
        for (int numero : numerosAleatorios) {
            System.out.println(numero);
        }

        // Solicita al usuario que ingrese un número entero
        System.out.println("Ingrese un número entero:");
        int numeroIngresado = scanner.nextInt();

        // Comprueba si el número ingresado está en la lista de números aleatorios
        boolean estaEnLaLista = numerosAleatorios.contains(numeroIngresado);

        // Muestra si el número ingresado está o no en la lista
        if (estaEnLaLista) {
            System.out.println("El número ingresado está en la lista.");
        } else {
            System.out.println("El número ingresado no está en la lista.");
        }

        // Calcula la suma de los números pares de la lista
        int sumaPares = 0;
        for (int numero : numerosAleatorios) {
            if (numero % 2 == 0) {
                sumaPares += numero;
            }
        }

        // Muestra la suma de los números pares
        System.out.println("Suma de los números pares:");
        System.out.println(sumaPares);

        // Calcula la media de los números impares de la lista
        double mediaImpares = 0;
        int cantidadImpares = 0;
        for (int numero : numerosAleatorios) {
            if (numero % 2 != 0) {
                mediaImpares += numero;
                cantidadImpares++;
            }
        }
        mediaImpares /= cantidadImpares;

        // Muestra la media de los números impares
        System.out.println("Media de los números impares:");
        System.out.println(mediaImpares);

        // Ordena la lista de números aleatorios en orden ascendente
        numerosAleatorios.sort(null);

        // Muestra la lista de números aleatorios ordenada
        System.out.println("Lista de números aleatorios ordenada:");
        for (int numero : numerosAleatorios) {
            System.out.println(numero);
        }

        // Invierte la lista de números aleatorios
        List<Integer> numerosInvertidos = new ArrayList<>();
        for (int i = numerosAleatorios.size() - 1; i >= 0; i--) {
            numerosInvertidos.add(numerosAleatorios.get(i));
        }

        // Muestra la lista de números aleatorios invertida
        System.out.println("Lista de números aleatorios invertida:");
        for (int numero : numerosInvertidos) {
            System.out.println(numero);
        }
    }
}
```

Explicación del código:

* Se importan las bibliotecas necesarias.
* Se crea la clase principal del programa.
* Se crea el método principal del programa.
* Se crea un objeto Scanner para leer datos de la consola.
* Se crea un objeto Random para generar números aleatorios.
* Se crea una lista de números aleatorios.
* Se muestra la lista de números aleatorios.
* Se solicita al usuario que ingrese un número entero.
* Se comprueba si el número ingresado está en la lista de números aleatorios.
* Se muestra si el número ingresado está o no en la lista.
* Se calcula la suma de los números pares de la lista.
* Se muestra la suma de los números pares.
* Se calcula la media de los números impares de la lista.
* Se muestra la media de los números impares.
* Se ordena la lista de números aleatorios en orden ascendente.
* Se muestra la lista de números aleatorios ordenada.
* Se invierte la lista de números aleatorios.
* Se muestra la lista de números aleatorios invertida.