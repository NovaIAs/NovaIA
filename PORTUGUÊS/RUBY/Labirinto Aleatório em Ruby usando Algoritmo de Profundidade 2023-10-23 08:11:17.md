Claro! Aqui está um exemplo de código em Ruby que gera um labirinto aleatório usando o algoritmo de profundidade:

```ruby
class Maze
  def initialize(rows, columns)
    @rows = rows
    @columns = columns
    @maze = Array.new(rows) { Array.new(columns, 1) }
  end

  def generate
    carve_passage(0, 0)
    @maze
  end

  private

  def carve_passage(row, column)
    directions = [[0, -1], [0, 1], [-1, 0], [1, 0]]
    directions.shuffle.each do |direction|
      next_row = row + direction[0] * 2
      next_column = column + direction[1] * 2
      if next_row.between?(0, @rows - 1) && next_column.between?(0, @columns - 1) && @maze[next_row][next_column] == 1
        @maze[row + direction[0]][column + direction[1]] = 0
        @maze[next_row][next_column] = 0
        carve_passage(next_row, next_column)
      end
    end
  end
end

def display_maze(maze)
  maze.each do |row|
    row.each do |cell|
      print cell == 1 ? "█" : " "
    end
    puts
  end
end

maze = Maze.new(15, 15)
generated_maze = maze.generate
display_maze(generated_maze)
```

Este código cria um labirinto aleatório usando o algoritmo de profundidade. A classe `Maze` é responsável por gerar o labirinto. O construtor `initialize` recebe o número de linhas e colunas do labirinto e cria uma matriz com todas as células preenchidas (valor 1).

O método `generate` é responsável por percorrer o labirinto e criar passagens aleatórias. O algoritmo começa na célula superior esquerda (0,0) e escolhe aleatoriamente uma direção (esquerda, direita, cima ou baixo). Se a célula adjacente naquela direção ainda não foi visitada, o algoritmo "escava" um caminho entre as células, definindo-as como passagem (valor 0) e chama recursivamente o método `carve_passage` para continuar o processo a partir da célula adjacente.

O método `display_maze` é responsável por exibir visualmente o labirinto gerado. Ele percorre a matriz do labirinto e imprime um caractere de parede (█) para células com valor 1 e um espaço em branco para células com valor 0.

Por fim, o código cria uma instância da classe `Maze` com as dimensões desejadas (15 linhas e 15 colunas), gera o labirinto chamando o método `generate` e exibe o labirinto chamando o método `display_maze`.