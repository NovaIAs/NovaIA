```java
// Clase que representa un grafo dirigido ponderado.
public class GrafoDirigidoPonderado {

    // Lista de vértices del grafo.
    private List<Vertice> vertices;

    // Lista de aristas del grafo.
    private List<Arista> aristas;

    // Constructor del grafo.
    public GrafoDirigidoPonderado() {
        this.vertices = new ArrayList<>();
        this.aristas = new ArrayList<>();
    }

    // Método para agregar un vértice al grafo.
    public void agregarVertice(Vertice vertice) {
        this.vertices.add(vertice);
    }

    // Método para agregar una arista al grafo.
    public void agregarArista(Arista arista) {
        this.aristas.add(arista);
    }

    // Método para obtener el peso total del grafo.
    public double getPesoTotal() {
        double pesoTotal = 0;
        for (Arista arista : this.aristas) {
            pesoTotal += arista.getPeso();
        }
        return pesoTotal;
    }

    // Método para encontrar el camino más corto entre dos vértices usando el algoritmo Dijkstra.
    public List<Vertice> encontrarCaminoMasCorto(Vertice origen, Vertice destino) {
        // Distancias desde el origen a cada vértice.
        Map<Vertice, Double> distancias = new HashMap<>();
        for (Vertice vertice : this.vertices) {
            distancias.put(vertice, Double.POSITIVE_INFINITY);
        }
        distancias.put(origen, 0.0);

        // Cola de prioridad para los vértices pendientes de visitar.
        PriorityQueue<Vertice> pendientes = new PriorityQueue<>((a, b) -> Double.compare(distancias.get(a), distancias.get(b)));
        pendientes.add(origen);

        // Mientras haya vértices pendientes de visitar.
        while (!pendientes.isEmpty()) {
            // Obtener el vértice con la distancia más corta.
            Vertice actual = pendientes.poll();

            // Si el vértice actual es el destino, hemos encontrado el camino más corto.
            if (actual.equals(destino)) {
                // Reconstruir el camino más corto.
                List<Vertice> camino = new ArrayList<>();
                Vertice verticeActual = destino;
                while (verticeActual != null) {
                    camino.add(verticeActual);
                    verticeActual = distancias.get(verticeActual);
                }
                Collections.reverse(camino);
                return camino;
            }

            // Relajar las aristas del vértice actual.
            for (Arista arista : this.aristas) {
                Vertice vecino = arista.getDestino();
                double nuevaDistancia = distancias.get(actual) + arista.getPeso();
                if (nuevaDistancia < distancias.get(vecino)) {
                    distancias.put(vecino, nuevaDistancia);
                    pendientes.add(vecino);
                }
            }
        }

        // Si no se encontró ningún camino, devolver una lista vacía.
        return new ArrayList<>();
    }
}

// Clase que representa un vértice del grafo.
public class Vertice {

    // Nombre del vértice.
    private String nombre;

    // Constructor del vértice.
    public Vertice(String nombre) {
        this.nombre = nombre;
    }

    // Método para obtener el nombre del vértice.
    public String getNombre() {
        return this.nombre;
    }

    // Método para comparar dos vértices.
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Vertice) {
            Vertice other = (Vertice) obj;
            return this.nombre.equals(other.nombre);
        }
        return false;
    }

    // Método para generar un código hash para el vértice.
    @Override
    public int hashCode() {
        return this.nombre.hashCode();
    }
}

// Clase que representa una arista del grafo.
public class Arista {

    // Vértice origen de la arista.
    private Vertice origen;

    // Vértice destino de la arista.
    private Vertice destino;

    // Peso de la arista.
    private double peso;

    // Constructor de la arista.
    public Arista(Vertice origen, Vertice destino, double peso) {
        this.origen = origen;
        this.destino = destino;
        this.peso = peso;
    }

    // Método para obtener el vértice origen de la arista.
    public Vertice getOrigen() {
        return this.origen;
    }

    // Método para obtener el vértice destino de la arista.
    public Vertice getDestino() {
        return this.destino;
    }

    // Método para obtener el peso de la arista.
    public double getPeso() {
        return this.peso;
    }
}

// Ejemplo de uso del grafo dirigido ponderado.
public class EjemploGrafoDirigidoPonderado {

    public static void main(String[] args) {
        // Crear un grafo dirigido ponderado.
        GrafoDirigidoPonderado grafo = new GrafoDirigidoPonderado();

        // Agregar los vértices al grafo.
        Vertice v1 = new Vertice("V1");
        Vertice v2 = new Vertice("V2");
        Vertice v3 = new Vertice("V3");
        Vertice v4 = new Vertice("V4");
        Vertice v5 = new Vertice("V5");
        grafo.agregarVertice(v1);
        grafo.agregarVertice(v2);
        grafo.agregarVertice(v3);
        grafo.agregarVertice(v4);
        grafo.agregarVertice(v5);

        // Agregar las aristas al grafo.
        Arista a1 = new Arista(v1, v2, 1.0);
        Arista a2 = new Arista(v2, v3, 2.0);
        Arista a3 = new Arista(v3, v4, 3.0);
        Arista a4 = new Arista(v4, v5, 4.0);
        Arista a5 = new Arista(v5, v1, 5.0);
        grafo.agregarArista(a1);
        grafo.agregarArista(a2);
        grafo.agregarArista(a3);
        grafo.agregarArista(a4);
        grafo.agregarArista(a5);

        // Obtener el peso total del grafo.
        double pesoTotal = grafo.getPesoTotal();
        System.out.println("Peso total del grafo: " + pesoTotal);

        // Encontrar el camino más corto entre dos vértices.
        List<Vertice> caminoMasCorto = grafo.encontrarCaminoMasCorto(v1, v5);
        System.out.println("Camino más corto entre V1 y V5:");
        for (Vertice vertice : caminoMasCorto) {
            System.out.print(vertice.getNombre() + " ");
        }
        System.out.println();
    }
}
```

Este código implementa un grafo dirigido ponderado en Java. Un grafo dirigido ponderado es una estructura de datos que representa un conjunto de vértices y aristas, donde cada arista tiene un peso asociado. El código incluye las clases `GrafoDirigidoPonderado`, `Vertice` y `Arista`, que representan el grafo, los vértices y las aristas, respectivamente. El código también incluye una clase de ejemplo, `EjemploGrafoDirigidoPonderado`, que muestra cómo crear un grafo dirigido ponderado y cómo encontrar el camino más corto entre dos vértices usando el algoritmo de Dijkstra.

El código es complejo y diferenciado porque implementa varias características:

* Representación de grafos dirigidos ponderados.
* Operaciones básicas de grafos, como agregar vértices y aristas.
* Cálculo del peso total del grafo.
* Encontrar el camino más corto entre dos vértices usando el algoritmo de Dijkstra.

El código está bien documentado y organizado, lo que lo hace fácil de entender y usar.