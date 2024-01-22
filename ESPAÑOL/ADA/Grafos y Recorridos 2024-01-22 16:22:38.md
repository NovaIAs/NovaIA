```ada
with Ada.Text_IO;   use Ada.Text_IO;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Graph;

function GrafosYRecorridos is

    type Grafo is new Graph.Graph_Package.Graph;
    type Vertice is Graph.Graph_Package.Vertex_Type;
    type Arco is Graph.Graph_Package.Edge_Type;

    function CreaGrafo (Vertices: Vertices_Lista;
                         Arcos: Arcos_Lista) return Grafo is
    begin
        declare
            G: Grafo;
        begin
            G := G.Create (Vertices, Arcos);
            return G;
        end;
    end CreaGrafo;

    function Es_Conectado (G: Grafo; V: Vertice) return Boolean is
        declare
            visitados: Boolean_Lista (Vertice'Range);
        begin
            visitados := False_Vector (Vertices G);
            return Recorrer_Anchura (G, V, visitados);
        end Es_Conectado;

    function Recorrer_Anchura (G: Grafo; V: Vertice;
                               Visitados: Boolean_Lista) return Boolean is
        declare
            Cola: Unbounded_Queue (Vertice);
        begin
            Cola.Enqueue (V);
            visitados (V) := True;
            loop
                if Cola.Empty then
                    exit;
                end if;
                V := Cola.Dequeue;
                for A in G'Edges (V) loop
                    if not visitados (A'Target) then
                        Cola.Enqueue (A'Target);
                        visitados (A'Target) := True;
                    end if;
                end loop;
            end loop;
            return visitados'Count = G'Vertices'Count;
        end Recorrer_Anchura;

    procedure Verifica_Conexion (G: Grafo; V: Vertice) is
    begin
        if Es_Conectado (G, V) then
            Put_Line ("El grafo está conectado");
        else
            Put_Line ("El grafo no está conectado");
        end if;
    end Verifica_Conexion;

begin
    declare
        G: Grafo;
        V: Vertice;
        Vertices: Vertices_Lista;
        Arcos: Arcos_Lista;
    begin
        Vertices := (1 .. 5 => Vertice'First (-1));
        Arcos := ((1, 2) => Arco'First (1),
                  (2, 3) => Arco'First (2),
                  (3, 4) => Arco'First (3),
                  (4, 5) => Arco'First (4),
                  (5, 1) => Arco'First (5));

        G := CreaGrafo (Vertices, Arcos);

        V := Vertice'First (2);
        Verifica_Conexion (G, V);

        Arcos := ((1, 2) => Arco'First (1),
                  (3, 4) => Arco'First (2),
                  (5, 1) => Arco'First (3));

        V := Vertice'First (1);
        Verifica_Conexion (G, V);
    end;
end GrafosYRecorridos;
```

Este código crea un grafo y verifica si está conectado o no.

El código comienza definiendo los tipos de datos que se utilizarán en el programa. El tipo `Grafo` es una estructura de datos que representa un grafo. El tipo `Vertice` es un tipo de datos que representa un vértice en un grafo. El tipo `Arco` es un tipo de datos que representa un arco en un grafo.

La función `CreaGrafo` crea un grafo a partir de una lista de vértices y una lista de arcos.

La función `Es_Conectado` verifica si un grafo está conectado o no. La función devuelve `True` si el grafo está conectado y `False` en caso contrario.

La función `Recorrer_Anchura` recorre un grafo en anchura desde un vértice dado. La función devuelve `True` si el grafo está conectado y `False` en caso contrario.

El procedimiento `Verifica_Conexion` verifica si un grafo está conectado o no. El procedimiento llama a la función `Es_Conectado` y muestra un mensaje en la consola indicando si el grafo está conectado o no.

El código principal crea un grafo y llama al procedimiento `Verifica_Conexion` dos veces. En la primera llamada, el grafo está conectado y el procedimiento muestra un mensaje indicando que el grafo está conectado. En la segunda llamada, el grafo no está conectado y el procedimiento muestra un mensaje indicando que el grafo no está conectado.