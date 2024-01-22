```java
import java.util.*;
import java.math.*;
import java.io.*;

public class CodigoComplejoEnJava {

    // Clase para representar un grafo
    public static class Grafo {
        private Map<Integer, List<Integer>> adyacencia;
        private int numVertices;

        public Grafo(int numVertices) {
            this.numVertices = numVertices;
            this.adyacencia = new HashMap<>();
            for (int i = 0; i < numVertices; i++) {
                this.adyacencia.put(i, new ArrayList<>());
            }
        }

        public void añadirArista(int v1, int v2) {
            this.adyacencia.get(v1).add(v2);
        }

        public List<Integer> obtenerAdyacentes(int v) {
            return this.adyacencia.get(v);
        }

        public int obtenerNumVertices() {
            return this.numVertices;
        }
    }

    // Clase para representar un árbol de expansión mínimo
    public static class ArbolExpansionMinimo {
        private Map<Integer, Integer> padre;
        private Map<Integer, Integer> peso;
        private Set<Integer> verticesVisitados;

        public ArbolExpansionMinimo(int numVertices) {
            this.padre = new HashMap<>();
            this.peso = new HashMap<>();
            this.verticesVisitados = new HashSet<>();
            for (int i = 0; i < numVertices; i++) {
                this.padre.put(i, null);
                this.peso.put(i, Integer.MAX_VALUE);
            }
        }

        public void añadirArista(int v1, int v2, int peso) {
            if (this.verticesVisitados.contains(v1) && this.verticesVisitados.contains(v2)) {
                if (this.peso.get(v1) + peso < this.peso.get(v2)) {
                    this.padre.put(v2, v1);
                    this.peso.put(v2, this.peso.get(v1) + peso);
                } else if (this.peso.get(v2) + peso < this.peso.get(v1)) {
                    this.padre.put(v1, v2);
                    this.peso.put(v1, this.peso.get(v2) + peso);
                }
            } else if (this.verticesVisitados.contains(v1)) {
                this.padre.put(v2, v1);
                this.peso.put(v2, this.peso.get(v1) + peso);
                this.verticesVisitados.add(v2);
            } else if (this.verticesVisitados.contains(v2)) {
                this.padre.put(v1, v2);
                this.peso.put(v1, this.peso.get(v2) + peso);
                this.verticesVisitados.add(v1);
            } else {
                this.padre.put(v1, null);
                this.peso.put(v1, 0);
                this.padre.put(v2, v1);
                this.peso.put(v2, peso);
                this.verticesVisitados.add(v1);
                this.verticesVisitados.add(v2);
            }
        }

        public int obtenerPesoTotal() {
            int pesoTotal = 0;
            for (Map.Entry<Integer, Integer> entry : this.peso.entrySet()) {