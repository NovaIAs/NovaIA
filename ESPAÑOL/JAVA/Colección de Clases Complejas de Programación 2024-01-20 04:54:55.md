```java
import java.util.*;
import java.io.*;

public class CodigoComplejo {

    // Clase para representar un nodo de un árbol binario
    public static class NodoArbolBinario {

        private int valor;
        private NodoArbolBinario izquierda;
        private NodoArbolBinario derecha;

        public NodoArbolBinario(int valor) {
            this.valor = valor;
            this.izquierda = null;
            this.derecha = null;
        }
    }

    // Clase para representar un grafo
    public static class Grafo {

        private Map<Integer, List<Integer>> adyacentes;

        public Grafo() {
            this.adyacentes = new HashMap<>();
        }

        public void agregarVertice(int vertice) {
            this.adyacentes.putIfAbsent(vertice, new ArrayList<>());
        }

        public void agregarArista(int origen, int destino) {
            this.adyacentes.get(origen).add(destino);
        }
    }

    // Clase para representar una tabla de símbolos
    public static class TablaSimbolos {

        private Map<String, Integer> simbolos;

        public TablaSimbolos() {
            this.simbolos = new HashMap<>();
        }

        public void agregarSimbolo(String nombre, int valor) {
            this.simbolos.put(nombre, valor);
        }

        public int obtenerValor(String nombre) {
            return this.simbolos.get(nombre);
        }
    }

    // Clase para representar una máquina de estados finitos
    public static class MaquinaEstadosFinitos {

        private Set<Integer> estados;
        private Set<Integer> estadosFinales;
        private Map<Integer, Map<Integer, Integer>> transiciones;
        private int estadoInicial;

        public MaquinaEstadosFinitos() {
            this.estados = new HashSet<>();
            this.estadosFinales = new HashSet<>();
            this.transiciones = new HashMap<>();
            this.estadoInicial = -1;
        }

        public void agregarEstado(int estado) {
            this.estados.add(estado);
        }

        public void agregarEstadoFinal(int estadoFinal) {
            this.estadosFinales.add(estadoFinal);
        }

        public void agregarTransicion(int estadoOrigen, int simbolo, int estadoDestino) {
            this.transiciones.get(estadoOrigen).put(simbolo, estadoDestino);
        }

        public void establecerEstadoInicial(int estadoInicial) {
            this.estadoInicial = estadoInicial;
        }

        public boolean reconocerPalabra(String palabra) {
            int estadoActual = this.estadoInicial;
            for (char simbolo : palabra.toCharArray()) {
                if (this.transiciones.get(estadoActual).containsKey(simbolo)) {
                    estadoActual = this.transiciones.get(estadoActual).get(simbolo);
                } else {
                    return false;
                }
            }
            return this.estadosFinales.contains(estadoActual);
        }
    }

    // Clase para representar un analizador sintáctico
    public static class AnalizadorSintactico {

        private TablaSimbolos simbolos;
        private String expresion;
        private int indice;

        public AnalizadorSintactico(TablaSimbolos simbolos, String expresion) {
            this.simbolos = simbolos;
            this.expresion = expresion;
            this.indice = 0;
        }

        public int evaluarExpresion() {
            int resultado = this.evaluarTermino();
            while (this.indice < this.expresion.length() && (this.expresion.charAt(this.indice) == '+' || this.expresion.charAt(this.indice) == '-')) {
                char operador = this.expresion.charAt(this.indice);
                this.indice++;
                int operandoDerecho = this.evaluarTermino();
                if (operador == '+') {
                    resultado += operandoDerecho;
                } else if (operador == '-') {
                    resultado -= operandoDerecho;
                }
            }
            return resultado;
        }

        private int evaluarTermino() {
            int resultado = this.evaluarFactor();
            while (this.indice < this.expresion.length() && (this.expresion.charAt(this.indice) == '*' || this.expresion.charAt(this.indice) == '/')) {
                char operador = this.expresion.charAt(this.indice);
                this.indice++;
                int operandoDerecho = this.evaluarFactor();
                if (operador == '*') {
                    resultado *= operandoDerecho;
                } else if (operador == '/') {
                    resultado /= operandoDerecho;
                }
            }
            return resultado;
        }

        private int evaluarFactor() {
            int resultado = 0;

            if (this.expresion.charAt(this.indice) == '(') {
                this.indice++;
                resultado = this.evaluarExpresion();
                this.indice++;