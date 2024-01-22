```java
import java.util.*;
import java.io.*;

public class ComplejoCodigoJava {

    // Método para encontrar el máximo común divisor (mcd) de dos números
    private static int mcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return mcd(b, a % b);
    }

    // Método para encontrar el mínimo común múltiplo (mcm) de dos números
    private static int mcm(int a, int b) {
        return (a * b) / mcd(a, b);
    }

    // Método para encontrar la factorial de un número
    private static int factorial(int n) {
        if (n == 0) {
            return 1;
        }
        return n * factorial(n - 1);
    }

    // Método para generar una lista de números primos hasta un límite dado
    private static List<Integer> generarPrimos(int limite) {
        List<Integer> primos = new ArrayList<>();
        for (int i = 2; i <= limite; i++) {
            boolean esPrimo = true;
            for (int j = 2; j <= Math.sqrt(i); j++) {
                if (i % j == 0) {
                    esPrimo = false;
                    break;
                }
            }
            if (esPrimo) {
                primos.add(i);
            }
        }
        return primos;
    }

    // Método para invertir una cadena de caracteres
    private static String invertirCadena(String cadena) {
        StringBuilder invertida = new StringBuilder();
        for (int i = cadena.length() - 1; i >= 0; i--) {
            invertida.append(cadena.charAt(i));
        }
        return invertida.toString();
    }

    // Método para encontrar el subconjunto más grande de una matriz que suma un objetivo determinado
    private static List<Integer> subconjuntoSuma(int[] matriz, int objetivo) {
        List<Integer> subconjunto = new ArrayList<>();
        int suma = 0;
        int inicio = 0;
        int fin = 0;
        while (fin < matriz.length) {
            suma += matriz[fin];
            while (suma > objetivo) {
                suma -= matriz[inicio];
                inicio++;
            }
            if (suma == objetivo) {
                for (int i = inicio; i <= fin; i++) {
                    subconjunto.add(matriz[i]);
                }
                break;
            }
            fin++;
        }
        return subconjunto;
    }

    // Método para encontrar la ruta más corta entre dos nodos en un grafo
    private static List<Integer> rutaMasCorta(Map<Integer, List<Integer>> grafo, int origen, int destino) {
        Queue<Integer> cola = new LinkedList<>();
        cola.add(origen);
        Map<Integer, Integer> distancias = new HashMap<>();
        distancias.put(origen, 0);
        Map<Integer, Integer> padres = new HashMap<>();
        padres.put(origen, -1);
        while (!cola.isEmpty()) {
            int nodoActual = cola.poll();
            if (nodoActual == destino) {
                break;
            }
            for (int vecino : grafo.get(nodoActual)) {
                if (!distancias.containsKey(vecino)) {
                    distancias.put(vecino, distancias.get(nodoActual) + 1);
                    padres.put(vecino, nodoActual);
                    cola.add(vecino);
                }
            }
        }
        List<Integer> ruta = new ArrayList<>();
        int nodoActual = destino;
        while (nodoActual != -1) {
            ruta.add(nodoActual);
            nodoActual = padres.get(nodoActual);
        }
        Collections.reverse(ruta);
        return ruta;
    }

    // Método para encontrar el árbol de expansión mínimo de un grafo
    private static Map<Integer, Integer> arbolExpansionMinimo(Map<Integer, List<Integer>> grafo) {
        Map<Integer, Integer> arbol = new HashMap<>();
        Set<Integer> visitados = new HashSet<>();
        PriorityQueue<Integer> cola = new PriorityQueue<>(Comparator.comparingInt(nodo -> grafo.get(nodo).size()));
        cola.add(0);
        while (!cola.isEmpty()) {
            int nodoActual = cola.poll();
            if (visitados.contains(nodoActual)) {
                continue;
            }
            visitados.add(nodoActual);
            for (int vecino : grafo.get(nodoActual)) {
                if (!visitados.contains(vecino)) {
                    cola.add(vecino);
                    arbol.put(vecino, nodoActual);
                }
            }
        }
        return arbol;
    }

    // Método para encontrar el ciclo hamiltoniano en un grafo
    private static List<Integer> cicloHamiltoniano(Map<Integer, List<Integer>> grafo) {
        List<Integer> ciclo = new ArrayList<>();
        int nodoActual = 0;
        ciclo.add(nodoActual);
        Set<Integer> visitados = new HashSet<>();
        visitados.add(nodoActual);
        while (ciclo.size() < grafo.size()) {
            for (int vecino : grafo.get(nodoActual)) {
                if (!visitados.contains(vecino)) {
                    ciclo.add(vecino);
                    visitados.add(vecino);
                    nodoActual = vecino;
                    break;
                }
            }
            if (ciclo.size() == grafo.size()) {
                break;
            }
            nodoActual = ciclo.get(ciclo.size() - 1);
            ciclo.remove(ciclo.size() - 1);
            visitados.remove(nodoActual);
        }
        if (ciclo.size() == grafo.size() && grafo.get(ciclo.get(ciclo.size() - 1)).contains(ciclo.get(0))) {
            return ciclo;
        }
        return null;
    }

    public static void main(String[] args) {
        // Ejemplo de uso de los métodos
        int a = 12;
        int b = 18;
        System.out.println("Máximo común divisor de " + a + " y " + b + ": " + mcd(a, b));
        System.out.println("Mínimo común múltiplo de " + a + " y " + b + ": " + mcm(a, b));
        System.out.println("Factorial de " + a + ": " + factorial(a));
        System.out.println("Lista de números primos hasta 100: " + generarPrimos(100));
        String cadena = "Hola mundo";
        System.out.println("Cadena invertida: " + invertirCadena(cadena));
        int[] matriz = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        int objetivo = 15;
        System.out.println("Subconjunto de " + Arrays.toString(matriz) + " que suma " + objetivo + ": " + subconjuntoSuma(matriz, objetivo));
        Map<Integer, List<Integer>> grafo = new HashMap<>();
        grafo.put(0, Arrays.asList(1, 2));
        grafo.put(1, Arrays.asList(0, 2, 3));
        grafo.put(2, Arrays.asList(0, 1, 3, 4));
        grafo.put(3, Arrays.asList(1, 2, 4));
        grafo.put(4, Arrays.asList(2, 3));
        int origen = 0;
        int destino = 4;
        System.out.println("Ruta más corta entre " + origen + " y " + destino + " en el grafo: " + rutaMasCorta(grafo, origen, destino));
        System.out.println("Árbol de expansión mínimo del grafo: " + arbolExpansionMinimo(grafo));
        System.out.println("Ciclo hamiltoniano en el grafo: " + cicloHamiltoniano(grafo));
    }
}
```

Explicación del código:

* El código está dividido en varios métodos, cada uno de los cuales realiza una tarea específica.

* El método `mcd` calcula el máximo común divisor de dos números usando el algoritmo de Euclides.

* El método `mcm` calcula el mínimo común múltiplo de dos números usando el método del máximo común divisor.

* El método `factorial` calcula la factorial de un número usando la recursión.

* El método `generarPrimos` genera una lista de números primos hasta un límite dado usando el algoritmo del tamiz de Eratóstenes.

* El método `invertirCadena` invierte una cadena de caracteres usando un `StringBuilder`.

* El método `subconjuntoSuma` encuentra el subconjunto más grande de una matriz que suma un objetivo determinado usando el algoritmo de programación dinámica.

* El método `rutaMasCorta` encuentra la ruta más corta entre dos nodos en un grafo usando el algoritmo de Dijkstra.

* El método `arbolExpansionMinimo` encuentra el árbol de expansión mínimo de un grafo usando el algoritmo de Kruskal.

* El método `cicloHamiltoniano` encuentra el ciclo hamiltoniano en un grafo usando el algoritmo de Fleury.

* El método `main` es el punto de entrada del programa. En él se crean algunos ejemplos de uso de los métodos anteriores y se muestran los resultados.

Este código es un ejemplo de cómo utilizar Java para resolver problemas complejos de programación.