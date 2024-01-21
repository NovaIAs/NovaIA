```java
import java.util.*;

public class CodigoComplejo {

    private static final int TAMANIO_MAXIMO = 100;
    private static final int NUMERO_MAXIMO = 1000;

    public static void main(String[] args) {
        // Crear una matriz de enteros con dimensiones aleatorias
        Random random = new Random();
        int filas = random.nextInt(TAMANIO_MAXIMO) + 1;
        int columnas = random.nextInt(TAMANIO_MAXIMO) + 1;
        int[][] matriz = new int[filas][columnas];

        // Llenar la matriz con números aleatorios
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                matriz[i][j] = random.nextInt(NUMERO_MAXIMO) + 1;
            }
        }

        // Imprimir la matriz
        System.out.println("Matriz:");
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                System.out.print(matriz[i][j] + " ");
            }
            System.out.println();
        }

        // Calcular la suma de todos los elementos de la matriz
        int suma = 0;
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                suma += matriz[i][j];
            }
        }

        // Calcular el promedio de todos los elementos de la matriz
        double promedio = (double) suma / (filas * columnas);

        // Calcular el elemento máximo de la matriz
        int maximo = Integer.MIN_VALUE;
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                if (matriz[i][j] > maximo) {
                    maximo = matriz[i][j];
                }
            }
        }

        // Calcular el elemento mínimo de la matriz
        int minimo = Integer.MAX_VALUE;
        for (int i = 0; i < filas; i++) {
            for (int j = 0; j < columnas; j++) {
                if (matriz[i][j] < minimo) {
                    minimo = matriz[i][j];
                }
            }
        }

        // Imprimir la suma, el promedio, el máximo y el mínimo de la matriz
        System.out.println("Suma: " + suma);
        System.out.println("Promedio: " + promedio);
        System.out.println("Máximo: " + maximo);
        System.out.println("Mínimo: " + minimo);
    }
}
```

Explicación:

Este código crea una matriz de enteros con dimensiones aleatorias, llena la matriz con números aleatorios, imprime la matriz, calcula la suma de todos los elementos de la matriz, calcula el promedio de todos los elementos de la matriz, calcula el elemento máximo de la matriz, calcula el elemento mínimo de la matriz e imprime la suma, el promedio, el máximo y el mínimo de la matriz.

El código utiliza la clase `Random` para generar números aleatorios. La clase `Random` se utiliza para generar números aleatorios de diferentes tipos, como enteros, dobles, booleanos y otros.

El código utiliza bucles `for` para recorrer la matriz y realizar cálculos sobre los elementos de la matriz. Los bucles `for` se utilizan para repetir un bloque de código un número específico de veces.

El código utiliza la clase `System` para imprimir la matriz y los resultados de los cálculos. La clase `System` se utiliza para imprimir información en la consola.

El código utiliza la variable `suma` para almacenar la suma de todos los elementos de la matriz. La variable `suma` se inicializa con el valor 0 y se incrementa en cada iteración del bucle `for` que recorre la matriz.

El código utiliza la variable `promedio` para almacenar el promedio de todos los elementos de la matriz. La variable `promedio` se calcula dividiendo la variable `suma` por el número de elementos de la matriz.

El código utiliza la variable `maximo` para almacenar el elemento máximo de la matriz. La variable `maximo` se inicializa con el valor mínimo posible para un entero y se actualiza en cada iteración del bucle `for` que recorre la matriz si el elemento actual de la matriz es mayor que el valor actual de la variable `maximo`.

El código utiliza la variable `minimo` para almacenar el elemento mínimo de la matriz. La variable `minimo` se inicializa con el valor máximo posible para un entero y se actualiza en cada iteración del bucle `for` que recorre la matriz si el elemento actual de la matriz es menor que el valor actual de la variable `minimo`.