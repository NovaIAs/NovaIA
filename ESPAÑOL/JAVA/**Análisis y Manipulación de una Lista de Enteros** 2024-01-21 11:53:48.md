```java
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Random;
import java.util.List;

public class CodigoComplejo {

    public static void main(String[] args) {
        Scanner entrada = new Scanner(System.in);
        Random aleatorio = new Random();

        // Creación de una lista de enteros
        List<Integer> listaEnteros = new ArrayList<>();

        // Bucle para llenar la lista con 10 números aleatorios entre 1 y 100
        for (int i = 0; i < 10; i++) {
            listaEnteros.add(aleatorio.nextInt(100) + 1);
        }

        // Impresión de la lista de enteros
        System.out.println("Lista de enteros:");
        for (Integer entero : listaEnteros) {
            System.out.println(entero);
        }

        // Bucle para calcular la suma de los enteros de la lista
        int sumaEnteros = 0;
        for (Integer entero : listaEnteros) {
            sumaEnteros += entero;
        }

        // Impresión de la suma de los enteros
        System.out.println("Suma de los enteros: " + sumaEnteros);

        // Bucle para calcular el promedio de los enteros de la lista
        double promedioEnteros = (double) sumaEnteros / listaEnteros.size();

        // Impresión del promedio de los enteros
        System.out.println("Promedio de los enteros: " + promedioEnteros);

        // Bucle para calcular el número máximo de la lista
        int maximoEntero = Integer.MIN_VALUE;
        for (Integer entero : listaEnteros) {
            if (entero > maximoEntero) {
                maximoEntero = entero;
            }
        }

        // Impresión del número máximo
        System.out.println("Número máximo: " + maximoEntero);

        // Bucle para calcular el número mínimo de la lista
        int minimoEntero = Integer.MAX_VALUE;
        for (Integer entero : listaEnteros) {
            if (entero < minimoEntero) {
                minimoEntero = entero;
            }
        }

        // Impresión del número mínimo
        System.out.println("Número mínimo: " + minimoEntero);

        // Bucle para encontrar el índice del número máximo en la lista
        int indiceMaximo = -1;
        for (int i = 0; i < listaEnteros.size(); i++) {
            if (listaEnteros.get(i) == maximoEntero) {
                indiceMaximo = i;
                break;
            }
        }

        // Impresión del índice del número máximo
        System.out.println("Índice del número máximo: " + indiceMaximo);

        // Bucle para encontrar el índice del número mínimo en la lista
        int indiceMinimo = -1;
        for (int i = 0; i < listaEnteros.size(); i++) {
            if (listaEnteros.get(i) == minimoEntero) {
                indiceMinimo = i;
                break;
            }
        }

        // Impresión del índice del número mínimo
        System.out.println("Índice del número mínimo: " + indiceMinimo);

        // Bucle para invertir el orden de la lista
        List<Integer> listaInvertida = new ArrayList<>();
        for (int i = listaEnteros.size() - 1; i >= 0; i--) {
            listaInvertida.add(listaEnteros.get(i));
        }

        // Impresión de la lista invertida
        System.out.println("Lista invertida:");
        for (Integer entero : listaInvertida) {
            System.out.println(entero);
        }

        // Bucle para ordenar la lista en orden ascendente
        listaEnteros.sort((a, b) -> a - b);

        // Impresión de la lista ordenada
        System.out.println("Lista ordenada:");
        for (Integer entero : listaEnteros) {
            System.out.println(entero);
        }

        // Bucle para eliminar los duplicados de la lista
        List<Integer> listaSinDuplicados = new ArrayList<>();
        for (Integer entero : listaEnteros) {
            if (!listaSinDuplicados.contains(entero)) {
                listaSinDuplicados.add(entero);
            }
        }

        // Impresión de la lista sin duplicados
        System.out.println("Lista sin duplicados:");
        for (Integer entero : listaSinDuplicados) {
            System.out.println(entero);
        }
    }
}
```

Explicación del código:

1. Creamos una lista de enteros `listaEnteros` y la llenamos con 10 números aleatorios entre 1 y 100.
2. Imprimimos la lista de enteros.
3. Calculamos la suma de los enteros de la lista y la imprimimos.
4. Calculamos el promedio de los enteros de la lista y lo imprimimos.
5. Calculamos el número máximo de la lista y lo imprimimos.
6. Calculamos el número mínimo de la lista y lo imprimimos.
7. Encontramos el índice del número máximo en la lista y lo imprimimos.
8. Encontramos el índice del número mínimo en la lista y lo imprimimos.
9. Invertimos el orden de la lista y la imprimimos.
10. Ordenamos la lista en orden ascendente y la imprimimos.
11. Eliminamos los duplicados de la lista y la imprimimos.