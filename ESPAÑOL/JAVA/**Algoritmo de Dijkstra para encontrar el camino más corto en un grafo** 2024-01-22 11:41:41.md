```java
import java.util.*;
import java.io.*;

// Clase principal
public class CodigoComplejo {

    // Método para encontrar el camino más corto entre dos nodos en un grafo
    public static List<Integer> encontrarCaminoMasCorto(Grafo grafo, int nodoOrigen, int nodoDestino) {
        // Inicializar la distancia mínima para todos los nodos a infinito
        Map<Integer, Integer> distanciaMinima = new HashMap<>();
        for (int nodo : grafo.getTodosLosNodos()) {
            distanciaMinima.put(nodo, Integer.MAX_VALUE);
        }

        // Establecer la distancia mínima del nodo origen a sí mismo en 0
        distanciaMinima.put(nodoOrigen, 0);

        // Cola de prioridad para mantener una lista de nodos a explorar
        PriorityQueue<NodoDistancia> colaDePrioridad = new PriorityQueue<>(Comparator.comparing(NodoDistancia::getDistancia));

        // Añadir el nodo origen a la cola de prioridad
        colaDePrioridad.add(new NodoDistancia(nodoOrigen, 0));

        // Mientras haya nodos en la cola de prioridad
        while (!colaDePrioridad.isEmpty()) {

            // Obtener el nodo con la distancia mínima de la cola de prioridad
            NodoDistancia nodoActual = colaDePrioridad.poll();

            // Si el nodo actual es el nodo destino, hemos encontrado el camino más corto
            if (nodoActual.getNodo() == nodoDestino) {
                return reconstruirCamino(grafo, nodoOrigen, nodoDestino, distanciaMinima);
            }

            // Explorar los vecinos del nodo actual
            for (Arista arista : grafo.getVecinos(nodoActual.getNodo())) {

                // Obtener el nodo vecino y la distancia entre el nodo actual y el nodo vecino
                int nodoVecino = arista.getOtroNodo(nodoActual.getNodo());
                int distancia = arista.getPeso();

                // Calcular la distancia total desde el nodo origen al nodo vecino a través del nodo actual
                int distanciaTotal = distanciaMinima.get(nodoActual.getNodo()) + distancia;

                // Si la distancia total es menor que la distancia mínima actual al nodo vecino, actualizar la distancia mínima al nodo vecino
                if (distanciaTotal < distanciaMinima.get(nodoVecino)) {
                    distanciaMinima.put(nodoVecino, distanciaTotal);

                    // Añadir el nodo vecino a la cola de prioridad
                    colaDePrioridad.add(new NodoDistancia(nodoVecino, distanciaTotal));
                }
            }
        }

        // Si no se ha encontrado ningún camino, devolver una lista vacía
        return Collections.emptyList();
    }

    // Método para reconstruir el camino más corto entre dos nodos en un grafo
    private static List<Integer> reconstruirCamino(Grafo grafo, int nodoOrigen, int nodoDestino, Map<Integer, Integer> distanciaMinima) {

        // Inicializar la lista del camino
        List<Integer> camino = new ArrayList<>();

        // Empezar desde el nodo destino y retroceder hasta el nodo origen
        int nodoActual = nodoDestino;
        camino.add(nodoActual);

        // Mientras el nodo actual no sea el nodo origen
        while (nodoActual != nodoOrigen) {

            // Obtener los vecinos del nodo actual
            List<Arista> vecinos = grafo.getVecinos(nodoActual);

            // Buscar el vecino que tenga la distancia mínima al nodo actual
            Arista aristaMinima = null;
            for (Arista arista : vecinos) {

                // Obtener el otro nodo de la arista
                int otroNodo = arista.getOtroNodo(nodoActual);

                // Si la distancia mínima al otro nodo es menor que la distancia mínima al nodo actual, actualizar la arista mínima
                if (distanciaMinima.get(otroNodo) < distanciaMinima.get(nodoActual)) {
                    aristaMinima = arista;
                }
            }

            // Obtener el nodo vecino de la arista mínima
            nodoActual = aristaMinima.getOtroNodo(nodoActual);

            // Añadir el nodo vecino al camino
            camino.add(nodoActual);
        }

        // Invertir el camino para obtener el camino correcto desde el nodo origen al nodo destino
        Collections.reverse(camino);

        // Devolver el camino
        return camino;
    }

    // Clase para representar un nodo y su distancia
    private static class NodoDistancia {

        private int nodo;
        private int distancia;

        public NodoDistancia(int nodo, int distancia) {
            this.nodo = nodo;
            this.distancia = distancia;
        }

        public int getNodo() {
            return nodo;
        }

        public int getDistancia() {
            return distancia;
        }
    }

    // Clase para representar un grafo
    public static class Grafo {

        // Mapa de nodos a sus vecinos
        private Map<Integer, List<Arista>> grafo;

        public Grafo() {
            grafo = new HashMap<>();
        }

        public void añadirNodo(int nodo) {
            grafo.put(nodo, new ArrayList<>());
        }

        public void añadirArista(int nodoOrigen, int nodoDestino, int peso) {
            grafo.get(nodoOrigen).add(new Arista(nodoOrigen, nodoDestino, peso));
        }

        public List<Integer> getTodosLosNodos() {
            return new ArrayList<>(grafo.keySet());
        }

        public List<Arista> getVecinos(int nodo) {
            return grafo.get(nodo);
        }
    }

    // Clase para representar una arista entre dos nodos
    private static class Arista {

        private int nodoOrigen;
        private int nodoDestino;
        private int peso;

        public Arista(int nodoOrigen, int nodoDestino, int peso) {
            this.nodoOrigen = nodoOrigen;
            this.nodoDestino = nodoDestino;
            this.peso = peso;
        }

        public int getNodoOrigen() {
            return nodoOrigen;
        }

        public int getNodoDestino() {
            return nodoDestino;
        }

        public int getPeso() {
            return peso;
        }

        public int getOtroNodo(int nodo) {
            if (nodo == nodoOrigen) {
                return nodoDestino;
            } else if (nodo == nodoDestino) {
                return nodoOrigen;
            } else {
                throw new IllegalArgumentException("El nodo no es ni el nodo origen ni el nodo destino");
            }
        }
    }

    // Método principal
    public static void main(String[] args) {

        // Crear un grafo
        Grafo grafo = new Grafo();

        // Añadir nodos al grafo
        grafo.añadirNodo(1);
        grafo.añadirNodo(2);
        grafo.añadirNodo(3);
        grafo.añadirNodo(4);
        grafo.añadirNodo(5);

        // Añadir aristas al grafo
        grafo.añadirArista(1, 2, 1);
        grafo.añadirArista(1, 3, 4);
        grafo.añadirArista(2, 3, 2);
        grafo.añadirArista(2, 4, 5);
        grafo.añadirArista(3, 4, 1);
        grafo.añadirArista(3, 5, 3);
        grafo.añadirArista(4, 5, 6);

        // Encontrar el camino más corto entre dos nodos
        List<Integer> camino = encontrarCaminoMasCorto(grafo, 1, 5);

        // Imprimir el camino más corto
        System.out.println("El camino más corto entre 1 y 5 es: " + camino);
    }
}
```