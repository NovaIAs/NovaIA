```java
// Programa para encontrar el camino más corto en un grafo ponderado utilizando el algoritmo de Dijkstra.

// Clase para representar un vértice del grafo.
class Vertice {
    private String nombre;
    private List<Arista> aristas;

    public Vertice(String nombre) {
        this.nombre = nombre;
        this.aristas = new ArrayList<>();
    }

    public String getNombre() {
        return nombre;
    }

    public List<Arista> getAristas() {
        return aristas;
    }

    public void añadirArista(Vertice destino, int peso) {
        aristas.add(new Arista(this, destino, peso));
    }
}

// Clase para representar una arista del grafo.
class Arista {
    private Vertice origen;
    private Vertice destino;
    private int peso;

    public Arista(Vertice origen, Vertice destino, int peso) {
        this.origen = origen;
        this.destino = destino;
        this.peso = peso;
    }

    public Vertice getOrigen() {
        return origen;
    }

    public Vertice getDestino() {
        return destino;
    }

    public int getPeso() {
        return peso;
    }
}

// Clase para representar el grafo ponderado.
class Grafo {
    private List<Vertice> vertices;

    public Grafo() {
        vertices = new ArrayList<>();
    }

    public List<Vertice> getVertices() {
        return vertices;
    }

    public void añadirVertice(Vertice vertice) {
        vertices.add(vertice);
    }
}

// Clase para implementar el algoritmo de Dijkstra.
class Dijkstra {
    private Grafo grafo;
    private Vertice origen;
    private Map<Vertice, Integer> distancias;
    private Set<Vertice> visitados;

    public Dijkstra(Grafo grafo, Vertice origen) {
        this.grafo = grafo;
        this.origen = origen;
        this.distancias = new HashMap<>();
        this.visitados = new HashSet<>();
    }

    public Map<Vertice, Integer> encontrarCaminosMasCortos() {
        // Inicializar las distancias a infinito para todos los vértices.
        for (Vertice vertice : grafo.getVertices()) {
            distancias.put(vertice, Integer.MAX_VALUE);
        }

        // La distancia del vértice de origen a sí mismo es 0.
        distancias.put(origen, 0);

        // Mientras haya vértices sin visitar, continuar el algoritmo.
        while (visitados.size() < grafo.getVertices().size()) {
            // Encontrar el vértice sin visitar con la distancia más corta.
            Vertice verticeActual = null;
            int distanciaMinima = Integer.MAX_VALUE;
            for (Vertice vertice : distancias.keySet()) {
                if (!visitados.contains(vertice) && distancias.get(vertice) < distanciaMinima) {
                    verticeActual = vertice;
                    distanciaMinima = distancias.get(vertice);
                }
            }

            // Si no se encontró ningún vértice sin visitar, terminar el algoritmo.
            if (verticeActual == null) {
                break;
            }

            // Marcar el vértice actual como visitado.
            visitados.add(verticeActual);

            // Actualizar las distancias de los vértices adyacentes al vértice actual.
            for (Arista arista : verticeActual.getAristas()) {
                Vertice verticeAdyacente = arista.getDestino();
                int distanciaActual = distancias.get(verticeActual);
                int distanciaNueva = distanciaActual + arista.getPeso();

                if (!visitados.contains(verticeAdyacente) && distanciaNueva < distancias.get(verticeAdyacente)) {
                    distancias.put(verticeAdyacente, distanciaNueva);
                }
            }
        }

        return distancias;
    }
}

// Clase principal para probar el algoritmo de Dijkstra.
public class Main {
    public static void main(String[] args) {
        // Crear un grafo ponderado.
        Grafo grafo = new Grafo();

        // Añadir vértices al grafo.
        Vertice verticeA = new Vertice("A");
        Vertice verticeB = new Vertice("B");
        Vertice verticeC = new Vertice("C");
        Vertice verticeD = new Vertice("D");
        Vertice verticeE = new Vertice("E");

        grafo.añadirVertice(verticeA);
        grafo.añadirVertice(verticeB);
        grafo.añadirVertice(verticeC);
        grafo.añadirVertice(verticeD);
        grafo.añadirVertice(verticeE);

        // Añadir aristas al grafo.
        verticeA.añadirArista(verticeB, 1);
        verticeA.añadirArista(verticeC, 4);
        verticeB.añadirArista(verticeC, 2);
        verticeB.añadirArista(verticeD, 2);
        verticeC.añadirArista(verticeD, 5);
        verticeC.añadirArista(verticeE, 1);
        verticeD.añadirArista(verticeE, 3);

        // Crear una instancia del algoritmo de Dijkstra.
        Dijkstra dijkstra = new Dijkstra(grafo, verticeA);

        // Encontrar los caminos más cortos desde el vértice de origen.
        Map<Vertice, Integer> distancias = dijkstra.encontrarCaminosMasCortos();

        // Imprimir los caminos más cortos.
        for (Vertice vertice : distancias.keySet()) {
            System.out.println("Distancia desde el vértice A hasta el vértice " + vertice.getNombre() + ": " + distancias.get(vertice));
        }
    }
}
```

Explicación del código:

- La clase Vertice representa un vértice del grafo y contiene una lista de aristas.


- La clase Arista representa una arista del grafo y contiene el vértice de origen, el vértice de destino y el peso de la arista.


- La clase Grafo representa el grafo ponderado y contiene una lista de vértices.


- La clase Dijkstra implementa el algoritmo de Dijkstra para encontrar los caminos más cortos en un grafo ponderado.


- La clase Main es la clase principal que se ejecuta cuando se ejecuta el programa y crea una instancia del grafo, añade los vértices y las aristas, y llama al algoritmo de Dijkstra para encontrar los caminos más cortos.