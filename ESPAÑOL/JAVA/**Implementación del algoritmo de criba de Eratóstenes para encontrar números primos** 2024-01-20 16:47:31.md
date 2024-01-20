```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {

    public static void main(String[] args) {
        // Esta lista contendrá los números primos
        List<Integer> numerosPrimos = new ArrayList<>();

        // Este método calcula todos los números primos hasta un número determinado
        for (int i = 2; i <= 1000; i++) {
            if (esPrimo(i)) {
                numerosPrimos.add(i);
            }
        }

        // Este método imprime todos los números primos
        for (int numeroPrimo : numerosPrimos) {
            System.out.println(numeroPrimo);
        }
    }

    // Este método determina si un número es primo
    private static boolean esPrimo(int numero) {
        // Si el número es 1, no es primo
        if (numero == 1) {
            return false;
        }

        // Si el número es 2, es primo
        if (numero == 2) {
            return true;
        }

        // Si el número es par, no es primo
        if (numero % 2 == 0) {
            return false;
        }

        // Comprobamos si el número es divisible por algún número impar entre 3 y la raíz cuadrada del número
        for (int i = 3; i <= Math.sqrt(numero); i += 2) {
            if (numero % i == 0) {
                return false;
            }
        }

        // Si el número no es divisible por ningún número impar entre 3 y la raíz cuadrada del número, es primo
        return true;
    }
}
```

Explicación del código:

* La clase `CodigoComplejo` contiene un método `main` que es el punto de entrada del programa.
* El método `main` crea una lista de números primos vacía.
* El método `main` llama al método `esPrimo` para calcular todos los números primos hasta 1000 y los añade a la lista de números primos.
* El método `main` imprime todos los números primos de la lista.
* El método `esPrimo` determina si un número es primo.
* El método `esPrimo` comprueba si el número es 1, 2 o par. Si es alguno de estos, no es primo.
* El método `esPrimo` comprueba si el número es divisible por algún número impar entre 3 y la raíz cuadrada del número. Si es divisible por alguno de estos números, no es primo.
* Si el número no es divisible por ningún número impar entre 3 y la raíz cuadrada del número, es primo.