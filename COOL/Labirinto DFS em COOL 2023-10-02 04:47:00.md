Claro! Aqui está um código em COOL que implementa um algoritmo de busca em profundidade (DFS) para encontrar um caminho em um labirinto:

```
class Labirinto {

  // Representação do labirinto como uma matriz de caracteres
  objeto labirinto : Array de Array de String <- nil

  // Tamanho do labirinto
  objeto tamanho : Int <- 0

  // Inicialização do labirinto
  método inicializar(n : Int) : Object {
    labirinto <- novo Array de Array de String(n)
    tamanho <- n
    
    // Preenche o labirinto com paredes
    para i <- 0 até n-1 faça
      labirinto[i] <- novo Array de String(n, "█")
    fim
      
    // Remove paredes aleatoriamente para criar caminhos
    para i <- 1 até n-2 faça
      para j <- 1 até n-2 faça
        se inteiroAleatório(2) = 0 então
          labirinto[i][j] <- " "
        fim
      fim
    fim

    // Define o ponto de partida e o destino
    labirinto[0][0] <- "S"
    labirinto[n-1][n-1] <- "D"

    retorna self
  }

  // Imprime o labirinto na tela
  método imprimir() : Object {
    para i <- 0 até tamanho-1 faça
      para j <- 0 até tamanho-1 faça
        imprima(labirinto[i][j])
      fim
      imprima("\n")
    fim

    retorna self
  }

  // Algoritmo de busca em profundidade (DFS)
  método buscarCaminho(x : Int, y : Int) : Boolean {
    // Verifica se chegou ao destino
    se labirinto[x][y] = "D" então
      retorne verdadeiro
    fim

    // Verifica se a posição atual é válida
    se x < 0 ou x >= tamanho ou y < 0 ou y >= tamanho ou labirinto[x][y] = "█" ou labirinto[x][y] = "X" então
      retorne falso
    fim

    // Marca a posição atual como visitada
    labirinto[x][y] <- "X"

    // Tenta mover para cima
    se buscarCaminho(x-1, y) então
      retorne verdadeiro
    fim

    // Tenta mover para a direita
    se buscarCaminho(x, y+1) então
      retorne verdadeiro
    fim

    // Tenta mover para baixo
    se buscarCaminho(x+1, y) então
      retorne verdadeiro
    fim

    // Tenta mover para a esquerda
    se buscarCaminho(x, y-1) então
      retorne verdadeiro
    fim

    // Se não encontrou um caminho, volta atrás
    labirinto[x][y] <- " "

    retorne falso
  }
}

// Exemplo de uso
objeto labirinto : Labirinto <- novo Labirinto().inicializar(10)
imprima("Labirinto inicial:\n")
labirinto.imprimir()

se labirinto.buscarCaminho(0, 0) então
  imprima("\nCaminho encontrado:\n")
  labirinto.imprimir()
senão
  imprima("\nNão foi possível encontrar um caminho.\n")
fim
```

Esse código cria um labirinto de tamanho 10x10, onde "█" representa uma parede, "S" o ponto de partida e "D" o destino. O algoritmo de busca em profundidade é usado para encontrar um caminho do ponto de partida ao destino, marcando as posições visitadas com "X". O resultado é impresso na tela, mostrando o caminho encontrado ou uma mensagem de que não foi possível encontrar um caminho.