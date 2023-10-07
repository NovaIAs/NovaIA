Claro! Aqui está um exemplo de um código complexo em Lua que realiza uma busca em largura (BFS) em um grafo representado por uma matriz de adjacência:

```lua
-- Função para realizar a busca em largura
function bfs(grafo, origem)
    -- Inicializa uma fila para armazenar os vértices a serem visitados
    local fila = {origem}
    
    -- Inicializa um vetor para armazenar os vértices já visitados
    local visitados = {}
    
    -- Marca o vértice de origem como visitado
    visitados[origem] = true
    
    -- Enquanto houver vértices na fila
    while #fila > 0 do
        -- Remove o primeiro vértice da fila
        local vertice = table.remove(fila, 1)
        
        -- Processa o vértice visitado
        print("Visitando vértice: " .. vertice)
        
        -- Obtém todos os vértices adjacentes ao vértice atual
        for adjacente, aresta in pairs(grafo[vertice]) do
            -- Se o vértice adjacente ainda não foi visitado
            if not visitados[adjacente] then
                -- Marca o vértice adjacente como visitado
                visitados[adjacente] = true
                
                -- Adiciona o vértice adjacente à fila
                table.insert(fila, adjacente)
            end
        end
    end
end

-- Exemplo de uso da função bfs

-- Definição do grafo
local grafo = {
    [1] = {2 = true, 3 = true},
    [2] = {1 = true, 4 = true},
    [3] = {1 = true, 4 = true},
    [4] = {2 = true, 3 = true, 5 = true},
    [5] = {4 = true}
}

-- Chama a função bfs a partir do vértice 1
bfs(grafo, 1)
```

Neste código, a função `bfs` implementa a busca em largura em um grafo representado por uma matriz de adjacência. A função recebe como parâmetros o grafo e o vértice de origem para iniciar a busca.

A busca em largura é realizada utilizando uma fila para armazenar os vértices a serem visitados. A função inicializa a fila com o vértice de origem e marca este vértice como visitado. Em seguida, um loop é executado até que não haja mais vértices na fila.

A cada iteração do loop, um vértice é removido da fila e processado. A função imprime uma mensagem indicando que o vértice está sendo visitado.

Em seguida, a função obtém todos os vértices adjacentes ao vértice atual e verifica se eles já foram visitados. Se um vértice adjacente não foi visitado, ele é marcado como visitado e adicionado à fila.

O exemplo de uso da função cria um grafo de exemplo e chama a função `bfs` a partir do vértice 1. Você pode modificar o grafo e o vértice de origem para testar diferentes casos.

Espero que este código atenda às suas expectativas! Se precisar de mais algum código ou tiver alguma dúvida, é só me perguntar!