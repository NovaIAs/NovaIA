```
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class CodigoComplejo {

    public static void main(String[] args) {

        // Crear una lista de números enteros
        List<Integer> numeros = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

        // Crear una función que devuelva el cuadrado de un número
        Function<Integer, Integer> cuadrado = x -> x * x;

        // Crear una función que devuelva si un número es par
        Predicate<Integer> esPar = x -> x % 2 == 0;

        // Filtrar los números pares de la lista
        List<Integer> numerosPares = numeros.stream()
                .filter(esPar)
                .collect(Collectors.toList());

        // Aplicar la función cuadrado a los números pares
        List<Integer> cuadradosNumerosPares = numerosPares.stream()
                .map(cuadrado)
                .collect(Collectors.toList());

        // Imprimir los cuadrados de los números pares
        System.out.println(cuadradosNumerosPares); // Esperado: [4, 16, 36, 64, 100]

        // Crear un mapa de los números enteros y sus cuadrados
        Map<Integer, Integer> mapaNumerosCuadrados = numeros.stream()
                .collect(Collectors.toMap(Function.identity(), cuadrado));

        // Imprimir el mapa
        System.out.println(mapaNumerosCuadrados); // Esperado: {1=1, 2=4, 3=9, 4=16, 5=25, 6=36, 7=49, 8=64, 9=81, 10=100}

    }

}
```

**Explicación:**

1. Creamos una lista de números enteros llamada `numeros`.
2. Creamos dos funciones: `cuadrado` y `esPar`. La función `cuadrado` devuelve el cuadrado de un número y la función `esPar` devuelve `true` si un número es par y `false` si no lo es.
3. Usamos la función `filter` para filtrar los números pares de la lista `numeros`.
4. Usamos la función `map` para aplicar la función `cuadrado` a los números pares.
5. Usamos la función `collect` para recopilar los resultados de la función `map` en una nueva lista llamada `cuadradosNumerosPares`.
6. Imprimimos la lista `cuadradosNumerosPares`.
7. Usamos la función `toMap` para crear un mapa de los números enteros y sus cuadrados.
8. Imprimimos el mapa.

Este código es complejo porque combina varias características avanzadas de Java, como flujos de datos, funciones lambda y recolección de flujos. Este código también es difícil de repetir porque utiliza una serie de características que no son comunes en otros lenguajes de programación.