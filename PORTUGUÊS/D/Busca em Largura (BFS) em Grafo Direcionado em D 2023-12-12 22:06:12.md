Certamente! Vou criar um exemplo de código complexo em D para você. Este código é uma implementação de um algoritmo de busca em largura (BFS) em um grafo direcionado. Vamos começar importando os módulos necessários:

```d
import std.stdio;
import std.algorithm;
import std.container;
```

Agora, vamos definir uma estrutura para representar um nó no grafo. Cada nó terá um valor e uma lista de seus vizinhos:

```d
struct Node(T)
{
    T value;
    NodeList!T neighbors;
}

struct NodeList(T)
{
    private T[] _data;
    
    void add(Node!T node)
    {
        _data ~= node;
    }
    
    T[] data()
    {
        return _data;
    }
}
```

Em seguida, vamos implementar a função de busca em largura:

```d
void bfs(T)(Node!T start)
{
    Queue!Node!T queue;
    HashSet!T visited;
    
    queue.insertBack(start);
    visited.insert(start.value);
    
    while (!queue.empty)
    {
        auto current = queue.front;
        queue.popFront;
        
        writeln("Visitando nó: ", current.value);
        
        auto neighbors = current.neighbors.data();
        foreach (neighbor; neighbors)
        {
            if (!visited.contains(neighbor.value))
            {
                visited.insert(neighbor.value);
                queue.insertBack(neighbor);
            }
        }
    }
}
```

Agora podemos criar um grafo de exemplo e realizar a busca em largura:

```d
void main()
{
    Node!int one = Node!int(1);
    Node!int two = Node!int(2);
    Node!int three = Node!int(3);
    Node!int four = Node!int(4);
    Node!int five = Node!int(5);
    
    one.neighbors.add(two);
    one.neighbors.add(three);
    two.neighbors.add(four);
    three.neighbors.add(five);
    
    bfs(one);
}
```

Neste exemplo, criamos um grafo com cinco nós e realizamos a busca em largura a partir do nó com valor 1. A cada visita, o valor do nó é exibido na tela. Você pode modificar este código para criar e testar grafos personalizados.

Espero que este exemplo complexo em D seja útil para você! Se você tiver mais alguma dúvida ou precisar de mais exemplos, estou aqui para ajudar.