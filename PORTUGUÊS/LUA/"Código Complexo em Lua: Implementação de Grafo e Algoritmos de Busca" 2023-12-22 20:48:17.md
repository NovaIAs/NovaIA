Claro! Aqui está um código complexo em Lua, que envolve a criação de uma estrutura de dados chamada "grafo" e a implementação de algoritmos de busca em largura e profundidade. O código a seguir utiliza tabelas Lua para representar o grafo e implementa os algoritmos BFS (Busca em Largura) e DFS (Busca em Profundidade) para percorrer o grafo.

```lua
-- Definindo a estrutura de dados do grafo
local Grafo = {}
Grafo.__index = Grafo

function Grafo.novo()
  local self = setmetatable({}, Grafo)
  self.vertices = {}
  return self
end

function Grafo:adicionarVertice(nome)
  self.vertices[nome] = {}
end

function Grafo:adicionarAresta(origem, destino)
  table.insert(self.vertices[origem], destino)
end

-- Algoritmo de busca em largura (BFS)
function Grafo:bfs(origem)
  local visitados = {}
  local fila = {origem}
  visitados[origem] = true

  while #fila > 0 do
    local vertice = table.remove(fila, 1)
    print("Visitando vertice: " .. vertice)

    for _, vizinho in ipairs(self.vertices[vertice]) do
      if not visitados[vizinho] then
        table.insert(fila, vizinho)
        visitados[vizinho] = true
      end
    end
  end
end

-- Algoritmo de busca em profundidade (DFS)
function Grafo:dfs(vertice, visitados)
  visitados[vertice] = true
  print("Visitando vertice: " .. vertice)

  for _, vizinho in ipairs(self.vertices[vertice]) do
    if not visitados[vizinho] then
      self:dfs(vizinho, visitados)
    end
  end
end

-- Exemplo de uso do código
local g = Grafo.novo()

-- Adicionando vértices ao grafo
g:adicionarVertice("A")
g:adicionarVertice("B")
g:adicionarVertice("C")
g:adicionarVertice("D")
g:adicionarVertice("E")

-- Adicionando arestas ao grafo
g:adicionarAresta("A", "B")
g:adicionarAresta("B", "C")
g:adicionarAresta("B", "D")
g:adicionarAresta("C", "D")
g:adicionarAresta("D", "E")

-- Executando a busca em largura a partir do vértice "A"
print("Busca em Largura:")
g:bfs("A")

-- Executando a busca em profundidade a partir do vértice "A"
print("Busca em Profundidade:")
g:dfs("A", {})
```

Neste código, a estrutura de dados "grafo" é implementada usando as tabelas Lua. A função `Grafo.novo()` cria um novo objeto de grafo, enquanto as funções `adicionarVertice` e `adicionarAresta` são usadas para adicionar vértices e arestas ao grafo, respectivamente.

Os algoritmos de busca em largura (BFS) e busca em profundidade (DFS) são implementados nas funções `bfs` e `dfs`, respectivamente. Ambos os algoritmos fazem uso de uma tabela `visitados` para rastrear os vértices já visitados durante a busca.

No exemplo de uso do código, são adicionados alguns vértices e arestas ao grafo, e então a busca em largura e profundidade é executada a partir do vértice "A", exibindo cada vértice visitado durante o percurso.

Espero que este código complexo seja útil para você! Se tiver mais alguma solicitação, é só pedir.