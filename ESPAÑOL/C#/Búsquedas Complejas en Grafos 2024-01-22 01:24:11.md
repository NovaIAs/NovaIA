```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejoCSharp
{
    // Definir una clase para representar un grafo
    public class Grafo
    {
        // Diccionario para almacenar los vértices del grafo
        private Dictionary<string, Vertice> vertices = new Dictionary<string, Vertice>();

        // Método para agregar un vértice al grafo
        public void AgregarVertice(string nombre)
        {
            // Crear un nuevo vértice con el nombre dado
            Vertice vertice = new Vertice(nombre);

            // Agregar el vértice al diccionario
            vertices.Add(nombre, vertice);
        }

        // Método para agregar un arco al grafo
        public void AgregarArco(string origen, string destino, int peso)
        {
            // Obtener los vértices de origen y destino
            Vertice verticeOrigen = vertices[origen];
            Vertice verticeDestino = vertices[destino];

            // Crear un nuevo arco con el origen, destino y peso dados
            Arco arco = new Arco(verticeOrigen, verticeDestino, peso);

            // Agregar el arco a la lista de arcos del vértice de origen
            verticeOrigen.Arcos.Add(arco);
        }

        // Método para realizar una búsqueda en profundidad en el grafo
        public List<string> BusquedaEnProfundidad(string verticeInicial)
        {
            // Crear una lista para almacenar los vértices visitados
            List<string> visitados = new List<string>();

            // Crear una pila para almacenar los vértices que aún no se han visitado
            Stack<Vertice> pila = new Stack<Vertice>();

            // Obtener el vértice inicial
            Vertice vertice = vertices[verticeInicial];

            // Marcar el vértice inicial como visitado
            visitados.Add(vertice.Nombre);

            // Agregar el vértice inicial a la pila
            pila.Push(vertice);

            // Mientras la pila no esté vacía
            while (pila.Count > 0)
            {
                // Obtener el vértice superior de la pila
                vertice = pila.Pop();

                // Iterar sobre los arcos del vértice
                foreach (Arco arco in vertice.Arcos)
                {
                    // Obtener el vértice destino del arco
                    Vertice verticeDestino = arco.Destino;

                    // Si el vértice destino no ha sido visitado
                    if (!visitados.Contains(verticeDestino.Nombre))
                    {
                        // Marcar el vértice destino como visitado
                        visitados.Add(verticeDestino.Nombre);

                        // Agregar el vértice destino a la pila
                        pila.Push(verticeDestino);
                    }
                }
            }

            // Devolver la lista de vértices visitados
            return visitados;
        }

        // Método para realizar una búsqueda en anchura en el grafo
        public List<string> BusquedaEnAnchura(string verticeInicial)
        {
            // Crear una lista para almacenar los vértices visitados
            List<string> visitados = new List<string>();

            // Crear una cola para almacenar los vértices que aún no se han visitado
            Queue<Vertice> cola = new Queue<Vertice>();

            // Obtener el vértice inicial
            Vertice vertice = vertices[verticeInicial];

            // Marcar el vértice inicial como visitado
            visitados.Add(vertice.Nombre);

            // Agregar el vértice inicial a la cola
            cola.Enqueue(vertice);

            // Mientras la cola no esté vacía
            while (cola.Count > 0)
            {
                // Obtener el vértice delantero de la cola
                vertice = cola.Dequeue();

                // Iterar sobre los arcos del vértice
                foreach (Arco arco in vertice.Arcos)
                {
                    // Obtener el vértice destino del arco
                    Vertice verticeDestino = arco.Destino;

                    // Si el vértice destino no ha sido visitado
                    if (!visitados.Contains(verticeDestino.Nombre))
                    {
                        // Marcar el vértice destino como visitado
                        visitados.Add(verticeDestino.Nombre);

                        // Agregar el vértice destino a la cola
                        cola.Enqueue(verticeDestino);
                    }
                }
            }

            // Devolver la lista de vértices visitados
            return visitados;
        }

        // Método para encontrar el camino más corto entre dos vértices
        public List<string> CaminoMasCorto(string origen, string destino)
        {
            // Crear una lista para almacenar el camino más corto
            List<string> camino = new List<string>();

            // Obtener los vértices de origen y destino
            Vertice verticeOrigen = vertices[origen];
            Vertice verticeDestino = vertices[destino];

            // Crear una cola de prioridad para almacenar los vértices a visitar
            PriorityQueue<Vertice, int> cola = new PriorityQueue<Vertice, int>();

            // Establecer la distancia del vértice de origen a 0
            verticeOrigen.Distancia = 0;

            // Agregar el vértice de origen a la cola de prioridad
            cola.Enqueue(verticeOrigen, 0);

            // Mientras la cola de prioridad no esté vacía
            while (cola.Count > 0)
            {
                // Obtener el vértice con la distancia más baja de la cola de prioridad