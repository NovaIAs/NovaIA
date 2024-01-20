```java
import java.util.*;

public class ProgramaComplejo {

    public static void main(String[] args) {
        // Crear una lista de elementos aleatorios
        List<Integer> lista = new ArrayList<>();
        Random random = new Random();
        for (int i = 0; i < 100; i++) {
            lista.add(random.nextInt(100));
        }

        // Ordenar la lista de forma ascendente
        Collections.sort(lista);

        // Crear un mapa para almacenar los elementos de la lista y su frecuencia
        Map<Integer, Integer> mapa = new HashMap<>();
        for (Integer elemento : lista) {
            if (mapa.containsKey(elemento)) {
                mapa.put(elemento, mapa.get(elemento) + 1);
            } else {
                mapa.put(elemento, 1);
            }
        }

        // Imprimir los elementos de la lista y su frecuencia
        for (Map.Entry<Integer, Integer> entrada : mapa.entrySet()) {
            System.out.println(entrada.getKey() + ": " + entrada.getValue());
        }

        // Crear un árbol binario de búsqueda para almacenar los elementos de la lista
        BinarySearchTree<Integer> arbol = new BinarySearchTree<>();
        for (Integer elemento : lista) {
            arbol.insert(elemento);
        }

        // Imprimir los elementos del árbol binario de búsqueda en orden ascendente
        arbol.printInorder();

        // Crear una red neuronal artificial para clasificar imágenes
        NeuralNetwork redNeuronal = new NeuralNetwork(100, 10);

        // Entrenar la red neuronal artificial con un conjunto de datos de imágenes
        redNeuronal.train(imagenes, etiquetas);

        // Evaluar la red neuronal artificial con otro conjunto de datos de imágenes
        double precision = redNeuronal.evaluate(imagenesEvaluacion, etiquetasEvaluacion);

        // Imprimir la precisión de la red neuronal artificial
        System.out.println("Precisión: " + precision);
    }

    // Clase que implementa un árbol binario de búsqueda
    private static class BinarySearchTree<T extends Comparable<T>> {

        private Node<T> root;

        public void insert(T value) {
            root = insert(root, value);
        }

        private Node<T> insert(Node<T> node, T value) {
            if (node == null) {
                return new Node<>(value);
            }

            if (value.compareTo(node.value) < 0) {
                node.left = insert(node.left, value);
            } else {
                node.right = insert(node.right, value);
            }

            return node;
        }

        public void printInorder() {
            printInorder(root);
        }

        private void printInorder(Node<T> node) {
            if (node == null) {
                return;
            }

            printInorder(node.left);
            System.out.println(node.value);
            printInorder(node.right);
        }

        private static class Node<T> {

            private T value;
            private Node<T> left;
            private Node<T> right;

            public Node(T value) {
                this.value = value;
            }
        }
    }

    // Clase que implementa una red neuronal artificial
    private static class NeuralNetwork {

        private int[] layers;
        private double[][] weights;
        private double[][] biases;

        public NeuralNetwork(int... layers) {
            this.layers = layers;

            weights = new double[layers.length - 1][];
            biases = new double[layers.length - 1][];

            for (int i = 1; i < layers.length; i++) {
                weights[i - 1] = new double[layers[i - 1]][layers[i]];
                biases[i - 1] = new double[layers[i]];
            }

            // Inicializar los pesos y los sesgos con valores aleatorios
            randomizeWeightsAndBiases();
        }

        public void train(double[][] inputs, double[][] outputs) {
            // Calcular los errores y los gradientes para cada capa
            double[][] errors = new double[layers.length][];
            double[][] gradients = new double[layers.length][];

            for (int i = 0; i < layers.length; i++) {
                errors[i] = new double[layers[i]];
                gradients[i] = new double[layers[i]];
            }

            // Realizar la propagación hacia atrás para actualizar los pesos y los sesgos
            for (int i = layers.length - 1; i >= 1; i--) {
                if (i == layers.length - 1) {
                    for (int j = 0; j < layers[i]; j++) {
                        errors[i][j] = outputs[j] - activations[i][j];
                        gradients[i][j] = errors[i][j] * activations[i][j] * (1 - activations[i][j]);
                    }
                } else {
                    for (int j = 0; j < layers[i