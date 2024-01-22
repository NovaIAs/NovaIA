```java
import java.util.*;
import java.io.*;
import java.math.*;

public class CodigoComplejo {

    // Método principal
    public static void main(String[] args) {
        // Crear un escáner para leer la entrada del usuario
        Scanner scanner = new Scanner(System.in);

        // Pedir al usuario que introduzca un número entero
        System.out.println("Introduce un número entero:");
        int numero = scanner.nextInt();

        // Crear una lista de números primos
        List<Integer> numerosPrimos = new ArrayList<>();

        // Añadir 2 a la lista de números primos
        numerosPrimos.add(2);

        // Iterar sobre los números desde 3 hasta el número introducido por el usuario
        for (int i = 3; i <= numero; i++) {
            // Comprobar si el número es primo
            boolean esPrimo = true;
            for (int j = 2; j <= Math.sqrt(i); j++) {
                if (i % j == 0) {
                    esPrimo = false;
                    break;
                }
            }

            // Si el número es primo, añadirlo a la lista de números primos
            if (esPrimo) {
                numerosPrimos.add(i);
            }
        }

        // Mostrar la lista de números primos
        System.out.println("Los números primos hasta " + numero + " son:");
        for (int numeroPrimo : numerosPrimos) {
            System.out.println(numeroPrimo);
        }
    }
}
```

Explicación del código:

* El código utiliza una clase llamada `CodigoComplejo` que contiene el método `main()`, que es el punto de entrada del programa.
* El método `main()` crea un escáner para leer la entrada del usuario, pide al usuario que introduzca un número entero y lo guarda en una variable llamada `numero`.
* A continuación, crea una lista de números primos llamada `numerosPrimos` y añade el número 2 a la lista.
* El código itera sobre los números desde 3 hasta el número introducido por el usuario, comprobando si cada número es primo.
* Para comprobar si un número es primo, el código itera sobre todos los números desde 2 hasta la raíz cuadrada del número.
* Si el número es divisible por alguno de estos números, no es primo y el código pasa al siguiente número.
* Si el número no es divisible por ninguno de estos números, es primo y el código lo añade a la lista de números primos.
* Finalmente, el código muestra la lista de números primos.