```java
import java.util.*;
import java.lang.*;
import java.math.*;

public class CodigoComplejo {

    // Función principal
    public static void main(String[] args) {
        // 1. Definir una lista de números aleatorios
        List<Integer> numerosAleatorios = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            numerosAleatorios.add((int) (Math.random() * 100));
        }

        // 2. Filtrar los números pares de la lista
        List<Integer> numerosPares = new ArrayList<>();
        for (int numero : numerosAleatorios) {
            if (numero % 2 == 0) {
                numerosPares.add(numero);
            }
        }

        // 3. Calcular la suma de los números pares
        int sumaNumerosPares = 0;
        for (int numero : numerosPares) {
            sumaNumerosPares += numero;
        }

        // 4. Ordenar la lista de números pares en orden descendente
        numerosPares.sort(Collections.reverseOrder());

        // 5. Calcular la mediana de la lista de números pares
        int medianaNumerosPares = 0;
        if (numerosPares.size() % 2 == 0) {
            medianaNumerosPares = (numerosPares.get(numerosPares.size() / 2) + numerosPares.get(numerosPares.size() / 2 - 1)) / 2;
        } else {
            medianaNumerosPares = numerosPares.get(numerosPares.size() / 2);
        }

        // 6. Calcular la desviación estándar de la lista de números pares
        double desviacionEstandarNumerosPares = 0;
        for (int numero : numerosPares) {
            desviacionEstandarNumerosPares += Math.pow(numero - medianaNumerosPares, 2);
        }
        desviacionEstandarNumerosPares = Math.sqrt(desviacionEstandarNumerosPares / (numerosPares.size() - 1));

        // 7. Imprimir los resultados
        System.out.println("Lista de números aleatorios:");
        System.out.println(numerosAleatorios);

        System.out.println("Lista de números pares:");
        System.out.println(numerosPares);

        System.out.println("Suma de los números pares:");
        System.out.println(sumaNumerosPares);

        System.out.println("Mediana de los números pares:");
        System.out.println(medianaNumerosPares);

        System.out.println("Desviación estándar de los números pares:");
        System.out.println(desviacionEstandarNumerosPares);
    }
}
```

Explicación del código:

1. Se define una lista de números aleatorios utilizando la clase `ArrayList`.
2. Se filtra la lista de números aleatorios para obtener una nueva lista que contiene solo los números pares.
3. Se calcula la suma de los números pares.
4. Se ordena la lista de números pares en orden descendente.
5. Se calcula la mediana de la lista de números pares.
6. Se calcula la desviación estándar de la lista de números pares.
7. Se imprimen los resultados en la consola.