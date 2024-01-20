```java
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

public class Complejo {

    private Random random = new Random();

    public List<Integer> generarNumerosAleatorios(int cantidad, int minimo, int maximo) {
        List<Integer> numerosAleatorios = new ArrayList<>();
        for (int i = 0; i < cantidad; i++) {
            int numero = random.nextInt(maximo - minimo + 1) + minimo;
            numerosAleatorios.add(numero);
        }
        return numerosAleatorios;
    }

    public List<Integer> obtenerNumerosPares(List<Integer> numeros) {
        return numeros.stream().filter(numero -> numero % 2 == 0).collect(Collectors.toList());
    }

    public List<Integer> obtenerNumerosImpares(List<Integer> numeros) {
        return numeros.stream().filter(numero -> numero % 2 != 0).collect(Collectors.toList());
    }

    public double calcularPromedio(List<Integer> numeros) {
        int suma = 0;
        for (int numero : numeros) {
            suma += numero;
        }
        return (double) suma / numeros.size();
    }

    public int encontrarMaximo(List<Integer> numeros) {
        int maximo = Integer.MIN_VALUE;
        for (int numero : numeros) {
            if (numero > maximo) {
                maximo = numero;
            }
        }
        return maximo;
    }

    public int encontrarMinimo(List<Integer> numeros) {
        int minimo = Integer.MAX_VALUE;
        for (int numero : numeros) {
            if (numero < minimo) {
                minimo = numero;
            }
        }
        return minimo;
    }

    public static void main(String[] args) {
        Complejo complejo = new Complejo();

        // Generar 10 números aleatorios entre 1 y 100
        List<Integer> numeros = complejo.generarNumerosAleatorios(10, 1, 100);

        // Obtener los números pares de la lista
        List<Integer> numerosPares = complejo.obtenerNumerosPares(numeros);

        // Obtener los números impares de la lista
        List<Integer> numerosImpares = complejo.obtenerNumerosImpares(numeros);

        // Calcular el promedio de la lista
        double promedio = complejo.calcularPromedio(numeros);

        // Encontrar el máximo de la lista
        int maximo = complejo.encontrarMaximo(numeros);

        // Encontrar el mínimo de la lista
        int minimo = complejo.encontrarMinimo(numeros);

        // Imprimir los resultados
        System.out.println("Números aleatorios: " + numeros);
        System.out.println("Números pares: " + numerosPares);
        System.out.println("Números impares: " + numerosImpares);
        System.out.println("Promedio: " + promedio);
        System.out.println("Máximo: " + maximo);
        System.out.println("Mínimo: " + minimo);
    }
}
```

Explicación del código:

* La clase `Complejo` tiene un atributo `random` que es un generador de números aleatorios.
* El método `generarNumerosAleatorios` genera una lista de números aleatorios entre un mínimo y un máximo especificados.
* El método `obtenerNumerosPares` obtiene los números pares de una lista de números.
* El método `obtenerNumerosImpares` obtiene los números impares de una lista de números.
* El método `calcularPromedio` calcula el promedio de una lista de números.
* El método `encontrarMaximo` encuentra el máximo de una lista de números.
* El método `encontrarMinimo` encuentra el mínimo de una lista de números.
* El método `main` es el punto de entrada del programa.

Este código genera una lista de 10 números aleatorios entre 1 y 100, obtiene los números pares e impares de la lista, calcula el promedio de la lista, encuentra el máximo y el mínimo de la lista, e imprime los resultados.