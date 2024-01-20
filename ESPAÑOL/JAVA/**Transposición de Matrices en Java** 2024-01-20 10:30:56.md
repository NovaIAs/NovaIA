```java
// Clase principal del programa
public class Programa {

    // Método principal del programa
    public static void main(String[] args) {

        // Creamos una matriz de números enteros de 3 filas y 4 columnas
        int[][] matriz = new int[3][4];

        // Rellenamos la matriz con números aleatorios
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[i].length; j++) {
                matriz[i][j] = (int) (Math.random() * 100);
            }
        }

        // Mostramos la matriz por consola
        System.out.println("Matriz original:");
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[i].length; j++) {
                System.out.print(matriz[i][j] + " ");
            }
            System.out.println();
        }

        // Realizamos la transposición de la matriz
        int[][] matrizTranspuesta = new int[matriz[0].length][matriz.length];
        for (int i = 0; i < matriz.length; i++) {
            for (int j = 0; j < matriz[i].length; j++) {
                matrizTranspuesta[j][i] = matriz[i][j];
            }
        }

        // Mostramos la matriz transpuesta por consola
        System.out.println("Matriz transpuesta:");
        for (int i = 0; i < matrizTranspuesta.length; i++) {
            for (int j = 0; j < matrizTranspuesta[i].length; j++) {
                System.out.print(matrizTranspuesta[i][j] + " ");
            }
            System.out.println();
        }

    }

}
```

Explicación del código:

* El programa crea una matriz de números enteros de 3 filas y 4 columnas, y la rellena con números aleatorios.
* A continuación, realiza la transposición de la matriz, es decir, intercambia las filas por las columnas.
* Finalmente, muestra la matriz original y la matriz transpuesta por consola.