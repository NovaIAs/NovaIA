```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Complejo {

    // Clase interna para representar un nodo del grafo
    private static class Nodo {
        private String nombre;
        private List<Nodo> vecinos;

        public Nodo(String nombre) {
            this.nombre = nombre;
            this.vecinos = new ArrayList<>();
        }

        public String getNombre() {
            return nombre;
        }

        public List<Nodo> getVecinos() {
            return vecinos;
        }

        public void agregarVecino(Nodo vecino) {
            vecinos.add(vecino);
        }
    }

    // Clase principal del grafo
    private Map<String, Nodo> nodos;

    public Complejo() {
        nodos = new HashMap<>();
    }

    // Método para agregar un nodo al grafo
    public void agregarNodo(String nombre) {
        Nodo nodo = new Nodo(nombre);
        nodos.put(nombre, nodo);
    }

    // Método para agregar una arista entre dos nodos del grafo
    public void agregarArista(String nodo1, String nodo2) {
        Nodo n1 = nodos.get(nodo1);
        Nodo n2 = nodos.get(nodo2);
        n1.agregarVecino(n2);
        n2.agregarVecino(n1);
    }

    // Método para realizar un recorrido en profundidad del grafo
    public List<String> recorridoEnProfundidad(String nodoInicial) {
        List<String> visitados = new ArrayList<>();
        recorridoEnProfundidadRecursivo(nodoInicial, visitados);
        return visitados;
    }

    // Método recursivo para realizar el recorrido en profundidad
    private void recorridoEnProfundidadRecursivo(String nodoActual, List<String> visitados) {
        Nodo nodo = nodos.get(nodoActual);
        visitados.add(nodo.getNombre());
        for (Nodo vecino : nodo.getVecinos()) {
            if (!visitados.contains(vecino.getNombre())) {
                recorridoEnProfundidadRecursivo(vecino.getNombre(), visitados);
            }
        }
    }

    // Método para realizar un recorrido en anchura del grafo
    public List<String> recorridoEnAnchura(String nodoInicial) {
        List<String> visitados = new ArrayList<>();
        Queue<String> cola = new LinkedList<>();
        cola.add(nodoInicial);
        while (!cola.isEmpty()) {
            String nodoActual = cola.poll();
            visitados.add(nodoActual);
            for (Nodo vecino : nodos.get(nodoActual).getVecinos()) {
                if (!visitados.contains(vecino.getNombre()) && !cola.contains(vecino.getNombre())) {
                    cola.add(vecino.getNombre());
                }
            }
        }
        return visitados;
    }

    // Método para encontrar el camino más corto entre dos nodos del grafo
    public List<String> caminoMasCorto(String nodoInicial, String nodoFinal) {
        Map<String, Integer> distancias = new HashMap<>();
        Map<String, String> predecesores = new HashMap<>();
        for (String nodo : nodos.keySet()) {
            distancias.put(nodo, Integer.MAX_VALUE);
            predecesores.put(nodo, null);
        }
        distancias.put(nodoInicial, 0);
        Queue<String> cola = new LinkedList<>();
        cola.add(nodoInicial);
        while (!cola.isEmpty()) {
            String nodoActual = cola.poll();
            for (Nodo vecino : nodos.get(nodoActual).getVecinos()) {
                int distanciaActual = distancias.get(nodoActual);
                int distanciaVecino = distancias.get(vecino.getNombre());
                if (distanciaActual + 1 < distanciaVecino) {
                    distancias.put(vecino.getNombre(), distanciaActual + 1);
                    predecesores.put(vecino.getNombre(), nodoActual);
                    cola.add(vecino.getNombre());
                }
            }
        }
        if (distancias.get(nodoFinal) == Integer.MAX_VALUE) {
            return null; // No hay camino entre los nodos
        }
        List<String> camino = new ArrayList<>();
        String nodoActual = nodoFinal;
        while (nodoActual != null) {
            camino.add(0, nodoActual);
            nodoActual = predecesores.get(nodoActual);
        }
        return camino;
    }

    // Método para encontrar todos los ciclos del grafo
    public List<List<String>> ciclos() {
        List<List<String>> ciclos = new ArrayList<>();
        for (String nodoInicial : nodos.keySet()) {
            List<List<String>> ciclosLocales = ciclosLocales(nodoInicial);
            ciclos.addAll(ciclosLocales);
        }
        return ciclos;
    }

    // Método recursivo para encontrar los ciclos locales de un nodo
    private List<List<String>> ciclosLocales(String nodoInicial) {
        List<List<String>> ciclos = new ArrayList<>();
        List<String> camino = new ArrayList<>();
        camino.add(nodoInicial);
        ciclosLocalesRecursivo(nodoInicial, camino, ciclos);
        return ciclos;
    }

    // Método recursivo para encontrar los ciclos locales de un nodo
    private void ciclosLocalesRecursivo(String nodoActual, List<String> camino, List<List<String>> ciclos) {
        for (Nodo vecino : nodos.get(nodoActual).getVecinos()) {
            if (camino.contains(vecino.getNombre())) {
                List<String> ciclo = new ArrayList<>(camino);
                ciclo.add(vecino.getNombre());
                ciclos.add(ciclo);
            } else {
                camino.add(vecino.getNombre());
                ciclosLocalesRecursivo(vecino.getNombre(), camino, ciclos);
                camino.remove(camino.size() - 1);
            }
        }
    }

    // Método para encontrar el árbol de expansión mínimo del grafo
    public Map<String, String> arbolExpansionMinimo() {
        Map<String, String> arbol = new HashMap<>();
        Set<String> visitados = new HashSet<>();
        String nodoInicial = nodos.keySet().iterator().next();
        visitados.add(nodoInicial);
        Queue<String> cola = new LinkedList<>();
        cola.add(nodoInicial);
        while (!cola.isEmpty()) {
            String nodoActual = cola.poll();
            for (Nodo vecino : nodos.get(nodoActual).getVecinos()) {
                if (!visit