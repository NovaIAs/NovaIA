Claro! Aqui está um código complexo em Ruby que gera um labirinto aleatório usando o algoritmo de criação de labirintos chamado Recursive Backtracking. Vamos explicar o código em detalhes.

```ruby
class Maze
  def initialize(width, height)
    @width = width
    @height = height
    @maze = Array.new(height) { Array.new(width, 1) }
  end

  def generate
    carve_passage(0, 0)
  end

  def display
    @maze.each do |row|
      row.each do |cell|
        print cell == 1 ? "█" : " "
      end
      puts
    end
  end

  private
  
  def carve_passage(x, y)
    @maze[y][x] = 0

    directions = [[0, 1], [1, 0], [0, -1], [-1, 0]].shuffle

    directions.each do |direction|
      dx, dy = direction
      nx, ny = x + dx, y + dy

      if ny.between?(0, @height - 1) && nx.between?(0, @width - 1) && @maze[ny][nx] == 1
        @maze[y + dy / 2][x + dx / 2] = 0
        carve_passage(nx, ny)
      end
    end
  end
end

maze = Maze.new(20, 10)
maze.generate
maze.display
```

Explicação:

1. A classe `Maze` é definida com um construtor que inicializa a largura (`width`) e altura (`height`) do labirinto, bem como a matriz `maze` que representa o labirinto.

2. O método `generate` é responsável por iniciar a geração do labirinto chamando o método `carve_passage` com as coordenadas iniciais (0, 0).

3. O método `display` percorre a matriz `maze` e imprime o labirinto na tela, usando "█" para representar paredes e espaços em branco para caminhos.

4. O método `carve_passage` recebe as coordenadas de um ponto no labirinto e utiliza o algoritmo de Recursive Backtracking para criar passagens no labirinto. Ele usa um array de direções (cima, direita, baixo, esquerda) que é embaralhado a cada chamada recursiva para criar um labirinto único.

5. Dentro do loop, o método verifica se a próxima coordenada (`nx`, `ny`) está dentro dos limites do labirinto e se a célula correspondente na matriz `maze` é uma parede (valor 1). Se sim, a célula intermediária entre a célula atual e a próxima célula é "escavada" (valor 0) e a função `carve_passage` é chamada recursivamente com as novas coordenadas.

6. No final, criamos uma instância da classe `Maze` com uma largura de 20 e altura de 10, geramos o labirinto e o exibimos na tela.

Espero que este código complexo atenda às suas expectativas!