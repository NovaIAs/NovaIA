```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {

    // Crear una clase interna para representar un nodo en un árbol binario
    private static class NodoArbol {
        int valor;
        NodoArbol izquierdo;
        NodoArbol derecho;

        public NodoArbol(int valor) {
            this.valor = valor;
            this.izquierdo = null;
            this.derecho = null;
        }
    }

    // Crear una clase para representar un grafo dirigido
    private static class Grafo {
        private Map<Integer, List<Integer>> adyacencia;

        public Grafo() {
            this.adyacencia = new HashMap<>();
        }

        public void agregarVertice(int vertice) {
            this.adyacencia.putIfAbsent(vertice, new ArrayList<>());
        }

        public void agregarArista(int origen, int destino) {
            this.adyacencia.getOrDefault(origen, new ArrayList<>()).add(destino);
        }

        public List<Integer> obtenerAdyacentes(int vertice) {
            return this.adyacencia.getOrDefault(vertice, new ArrayList<>());
        }
    }

    // Crear una clase para representar un algoritmo genético
    private static class AlgoritmoGenetico {

        private Poblacion poblacion;
        private FuncionObjetivo funcionObjetivo;
        private double tasaMutacion;
        private double tasaCruce;

        public AlgoritmoGenetico(Poblacion poblacion, FuncionObjetivo funcionObjetivo,
                                 double tasaMutacion, double tasaCruce) {
            this.poblacion = poblacion;
            this.funcionObjetivo = funcionObjetivo;
            this.tasaMutacion = tasaMutacion;
            this.tasaCruce = tasaCruce;
        }

        public Poblacion ejecutar(int numGeneraciones) {
            for (int i = 0; i < numGeneraciones; i++) {
                Poblacion nuevaPoblacion = new Poblacion();

                // Seleccionar padres (torneo binario)
                for (int j = 0; j < this.poblacion.getTamanio(); j++) {
                    Individuo padre1 = this.poblacion.seleccionarIndividuo();
                    Individuo padre2 = this.poblacion.seleccionarIndividuo();
                    while (padre1 == padre2) {
                        padre2 = this.poblacion.seleccionarIndividuo();
                    }

                    // Cruzar padres para crear nuevos individuos
                    Individuo hijo1;
                    Individuo hijo2;
                    if (Math.random() < this.tasaCruce) {
                        hijos = this.cruzarPadres(padre1, padre2);
                        hijo1 = hijos[0];
                        hijo2 = hijos[1];
                    } else {
                        hijo1 = padre1;
                        hijo2 = padre2;
                    }

                    // Mutar hijos
                    if (Math.random() < this.tasaMutacion) {
                        this.mutarIndividuo(hijo1);
                    }
                    if (Math.random() < this.tasaMutacion) {
                        this.mutarIndividuo(hijo2);
                    }

                    // Agregar hijos a la nueva población
                    nuevaPoblacion.agregarIndividuo(hijo1);
                    nuevaPoblacion.agregarIndividuo(hijo2);
                }

                // Reemplazar población actual con nueva población
                this.poblacion = nuevaPoblacion;
            }

            return this.poblacion;
        }

        private Individuo[] cruzarPadres(Individuo padre1, Individuo padre2) {
            // Seleccionar punto de cruce aleatorio
            int puntoCruce = (int) (Math.random() * (padre