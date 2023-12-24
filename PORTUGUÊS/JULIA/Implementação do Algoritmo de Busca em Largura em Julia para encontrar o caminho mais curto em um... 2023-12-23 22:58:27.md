Claro! Aqui está um código complexo em Julia, que implementa o algoritmo de busca em largura (BFS) para encontrar o caminho mais curto em um labirinto. O código está devidamente comentado para facilitar a compreensão:

```julia
# Definindo a estrutura do nó do labirinto
struct MazeNode
    x::Int64  # Coordenada x do nó
    y::Int64  # Coordenada y do nó
    dist::Int64  # Distância do nó inicial até este nó
    parent::Union{Nothing, MazeNode}  # Nó pai deste nó
end

# Função para verificar se uma posição está dentro dos limites do labirinto
function is_valid(maze, x, y)
    return x >= 1 && x <= size(maze, 1) && y >= 1 && y <= size(maze, 2) && maze[x, y] != '#'
end

# Função para encontrar o caminho mais curto em um labirinto
function find_shortest_path(maze, start, goal)
    # Definindo as direções possíveis: cima, baixo, esquerda, direita
    dx = [0, 0, -1, 1]
    dy = [-1, 1, 0, 0]
    
    # Definindo a fila para a busca em largura
    queue = [MazeNode(start[1], start[2], 0, nothing)]
    
    # Definindo o labirinto visitado
    visited = falses(size(maze))
    visited[start[1], start[2]] = true
    
    # Enquanto a fila não estiver vazia
    while !isempty(queue)
        node = dequeue!(queue)  # Remover o primeiro nó da fila
        
        # Se o nó atual for o objetivo, retornar o caminho encontrado
        if (node.x, node.y) == goal
            path = [(node.x, node.y)]
            while node.parent !== nothing
                node = node.parent
                pushfirst!(path, (node.x, node.y))
            end
            return path
        end
        
        # Percorrer todas as direções possíveis
        for i in 1:length(dx)
            new_x = node.x + dx[i]
            new_y = node.y + dy[i]
            
            # Verificar se a nova posição é válida e ainda não foi visitada
            if is_valid(maze, new_x, new_y) && !visited[new_x, new_y]
                enqueue!(queue, MazeNode(new_x, new_y, node.dist + 1, node))
                visited[new_x, new_y] = true
            end
        end
    end
    
    return nothing  # Se não foi possível encontrar o caminho
end

# Exemplo de uso

# Definindo o labirinto
maze = [
    "#S########",
    "#........#",
    "#.######.#",
    "#........#",
    "#.######.#",
    "#........#",
    "#.######.#",
    "#.......##",
    "#......G.#",
    "##########"
]

# Encontrando o caminho mais curto do ponto S ao ponto G
start = (1, 2)
goal = (9, 8)
path = find_shortest_path(maze, start, goal)

# Imprimindo o caminho encontrado
if path !== nothing
    println("Caminho encontrado:")
    for (x, y) in path
        println("($x, $y)")
    end
else
    println("Não foi possível encontrar um caminho.")
end
```

Neste código, o labirinto é representado como uma matriz de caracteres, onde '#' representa uma parede, '.' representa um caminho livre e 'S' e 'G' representam o ponto de partida e o objetivo, respectivamente. O algoritmo de busca em largura percorre o labirinto, expandindo os nós adjacentes e mantendo uma fila para armazenar os nós a serem explorados. O algoritmo continua até encontrar o objetivo ou até que não haja mais nós para explorar. Se um caminho é encontrado, o código retorna as coordenadas do caminho mais curto. Caso contrário, retorna um valor nulo.