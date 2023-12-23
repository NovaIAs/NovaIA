Claro! Aqui está um código complexo em COOL, que utiliza uma estrutura de dados avançada e implementa um algoritmo de busca em largura para encontrar o caminho mais curto em um labirinto representado por uma matriz bidimensional.

```
class Labirinto {

    // Definindo as constantes para representar as direções de movimento no labirinto
    const DIREITA : Int <- 0;
    const BAIXO : Int <- 1;
    const ESQUERDA : Int <- 2;
    const CIMA : Int <- 3;

    // Definindo a estrutura de dados para representar um ponto no labirinto
    class Ponto {

        x : Int;
        y : Int;
        direcao : Int;

        // Construtor
        create(x : Int, y : Int, direcao : Int) : Ponto {
            {
                self.x <- x;
                self.y <- y;
                self.direcao <- direcao;
                self;
            }
        }
    }

    // Método para encontrar o caminho mais curto no labirinto
    metodo encontrarCaminho(labirinto : Array of Array of Int, inicio : Ponto, fim : Ponto) : List of Int {
        {
            // Criando uma fila para armazenar os pontos a serem explorados
            fila : List of Ponto <- nil;
            fila <- fila.append(inicio);

            // Criando uma matriz para marcar os pontos já visitados
            visitado : Array of Array of Bool <- new Array of Bool {labirinto.length, labirinto[0].length};
            visitado[inicio.x][inicio.y] <- true;

            // Criando uma matriz para armazenar as direções percorridas até cada ponto
            direcaoPercorrida : Array of Array of Int <- new Array of Int {labirinto.length, labirinto[0].length};

            // Enquanto a fila não estiver vazia
            enquanto (not fila.isEmpty()) faca {
                pontoAtual : Ponto <- fila.removeFirst();

                // Verificando se o ponto atual é o ponto de destino
                se (pontoAtual.x = fim.x) e (pontoAtual.y = fim.y) entao
                    retorna reconstruirCaminho(direcaoPercorrida, pontoAtual);

                // Explorando os pontos vizinhos
                para direcao em 0..3 faca {
                    novoPonto : Ponto <- moverPonto(pontoAtual, direcao, labirinto);
                    // Verificando se o novo ponto é válido e ainda não foi visitado
                    se pontoValido(novoPonto, labirinto) e (not visitado[novoPonto.x][novoPonto.y]) entao {
                        visitado[novoPonto.x][novoPonto.y] <- true;
                        direcaoPercorrida[novoPonto.x][novoPonto.y] <- direcao;
                        fila <- fila.append(novoPonto);
                    }
                }
            }

            // Se nenhum caminho foi encontrado, retorna uma lista vazia
            retorna nil;
        }
    }

    // Método auxiliar para mover um ponto na direção especificada
    metodo moverPonto(ponto : Ponto, direcao : Int, labirinto : Array of Array of Int) : Ponto {
        {
            novoX : Int <- ponto.x;
            novoY : Int <- ponto.y;

            // Movendo o ponto na direção especificada
            se direcao = DIREITA e (novoY + 1 < labirinto[0].length) entao
                novoY <- novoY + 1;
            senao se direcao = BAIXO e (novoX + 1 < labirinto.length) entao
                novoX <- novoX + 1;
            senao se direcao = ESQUERDA e (novoY - 1 >= 0) entao
                novoY <- novoY - 1;
            senao se direcao = CIMA e (novoX - 1 >= 0) entao
                novoX <- novoX - 1;

            // Retornando o novo ponto
            retorna Ponto.create(novoX, novoY, direcao);
        }
    }

    // Método auxiliar para verificar se um ponto é válido dentro do labirinto
    metodo pontoValido(ponto : Ponto, labirinto : Array of Array of Int) : Bool {
        {
            // Verificando se as coordenadas do ponto estão dentro dos limites do labirinto
            retorna (ponto.x >= 0) e (ponto.x < labirinto.length) e
                    (ponto.y >= 0) e (ponto.y < labirinto[0].length) e
                    (labirinto[ponto.x][ponto.y] ≠ 1);
        }
    }

    // Método auxiliar para reconstruir o caminho percorrido a partir das direções percorridas
    metodo reconstruirCaminho(direcaoPercorrida : Array of Array of Int, ponto : Ponto) : List of Int {
        {
            caminho : List of Int <- nil;

            // Reconstruindo o caminho a partir do ponto de destino até o ponto de início
            enquanto (ponto.direcao ≠ -1) faca {
                caminho <- caminho.add(ponto.direcao);
                ponto <- moverPonto(ponto, (ponto.direcao + 2) mod 4, direcaoPercorrida);
            }

            // Invertendo o caminho para ficar na ordem correta
            caminho <- caminho.reverse();

            // Retornando o caminho
            retorna caminho;
        }
    }
}

// Código principal
{
    // Definindo o labirinto
    labirinto : Array of Array of Int <- new Array of Int {5, 5};
    labirinto[0] <- [0, 1, 0, 0, 0];
    labirinto[1] <- [0, 1, 0, 1, 0];
    labirinto[2] <- [0, 0, 0, 1, 0];
    labirinto[3] <- [0, 1, 1, 1, 0];
    labirinto[4] <- [0, 0, 0, 0, 0];

    // Definindo o ponto de início e o ponto de destino
    inicio : Labirinto.Ponto <- Labirinto.Ponto.create(0, 0, -1);
    fim : Labirinto.Ponto <- Labirinto.Ponto.create(4, 4, -1);

    // Criando uma instância da classe Labirinto
    lab : Labirinto <- new Labirinto;

    // Encontrando o caminho mais curto no labirinto
    caminhoMaisCurto : List of Int <- lab.encontrarCaminho(labirinto, inicio, fim);

    // Imprimindo o caminho mais curto
    se caminhoMaisCurto ≠ nil entao
        para direcao em caminhoMaisCurto faca
            imprime direcao;
        imprime "\n";
    senao
        imprime "Nenhum caminho encontrado.\n";
}
```

Este código em COOL implementa um algoritmo de busca em largura para encontrar o caminho mais curto em um labirinto representado por uma matriz bidimensional. A classe `Labirinto` possui um método `encontrarCaminho` que recebe o labirinto, o ponto de início e o ponto de destino como argumentos e retorna uma lista de inteiros representando as direções percorridas para chegar ao destino.

O algoritmo utiliza uma fila para armazenar os pontos a serem explorados e uma matriz para marcar os pontos já visitados. Ele explora os pontos vizinhos do ponto atual, verificando se eles são válidos e ainda não foram visitados. O caminho percorrido até cada ponto é armazenado em uma matriz de direções percorridas.

Após encontrar o caminho mais curto, o código imprime as direções percorridas para chegar ao destino. Se nenhum caminho for encontrado, é exibida uma mensagem informando que nenhum caminho foi encontrado.

Espero que este código complexo em COOL atenda às suas necessidades!