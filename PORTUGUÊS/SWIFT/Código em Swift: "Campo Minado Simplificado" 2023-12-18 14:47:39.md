Certamente! Aqui está um código complexo em Swift que implementa uma versão simplificada do jogo "Campo Minado":

```swift
import Foundation

// Definindo a estrutura do tabuleiro e das células
struct Celula {
    var valor: Int
    var revelada: Bool
    var mina: Bool
}

// Função para gerar um tabuleiro aleatório
func gerarTabuleiro(linhas: Int, colunas: Int) -> [[Celula]] {
    var tabuleiro: [[Celula]] = Array(repeating: Array(repeating: Celula(valor: 0, revelada: false, mina: false), count: colunas), count: linhas)
    
    // Distribui minas aleatoriamente no tabuleiro
    var minasRestantes = min(linhas * colunas / 5, linhas * colunas - 1)
    
    while minasRestantes > 0 {
        let linhaAleatoria = Int.random(in: 0..<linhas)
        let colunaAleatoria = Int.random(in: 0..<colunas)
        
        if !tabuleiro[linhaAleatoria][colunaAleatoria].mina {
            tabuleiro[linhaAleatoria][colunaAleatoria].mina = true
            minasRestantes -= 1
        }
    }
    
    // Calcula os valores das células adjacentes às minas
    for linha in 0..<linhas {
        for coluna in 0..<colunas {
            if tabuleiro[linha][coluna].mina {
                for i in -1...1 {
                    for j in -1...1 {
                        if linha+i >= 0 && linha+i < linhas && coluna+j >= 0 && coluna+j < colunas && !tabuleiro[linha+i][coluna+j].mina {
                            tabuleiro[linha+i][coluna+j].valor += 1
                        }
                    }
                }
            }
        }
    }
    
    return tabuleiro
}

// Função para exibir o tabuleiro na tela
func exibirTabuleiro(tabuleiro: [[Celula]]) {
    for linha in tabuleiro {
        var linhaString = ""
        for celula in linha {
            if celula.revelada {
                if celula.mina {
                    linhaString += "X "
                } else {
                    linhaString += "\(celula.valor) "
                }
            } else {
                linhaString += "- "
            }
        }
        print(linhaString)
    }
}

// Função para revelar uma célula e verificar se o jogador ganhou ou perdeu
func revelarCelula(tabuleiro: inout [[Celula]], linha: Int, coluna: Int) {
    if tabuleiro[linha][coluna].revelada {
        return
    }
    
    tabuleiro[linha][coluna].revelada = true
    
    if tabuleiro[linha][coluna].mina {
        print("Você perdeu!")
        exibirTabuleiro(tabuleiro: tabuleiro)
        return
    }
    
    if tabuleiro[linha][coluna].valor == 0 {
        for i in -1...1 {
            for j in -1...1 {
                if linha+i >= 0 && linha+i < tabuleiro.count && coluna+j >= 0 && coluna+j < tabuleiro[0].count {
                    revelarCelula(tabuleiro: &tabuleiro, linha: linha+i, coluna: coluna+j)
                }
            }
        }
    }
    
    var todasReveladas = true
    for linha in tabuleiro {
        for celula in linha {
            if !celula.revelada && !celula.mina {
                todasReveladas = false
                break
            }
        }
    }
    
    if todasReveladas {
        print("Você ganhou!")
        exibirTabuleiro(tabuleiro: tabuleiro)
        return
    }
    
    exibirTabuleiro(tabuleiro: tabuleiro)
}

// Exemplo de uso

// Gera um tabuleiro 5x5
let tabuleiro = gerarTabuleiro(linhas: 5, colunas: 5)

// Exibe o tabuleiro inicialmente oculto
exibirTabuleiro(tabuleiro: tabuleiro)

// Revela uma célula (linha 2, coluna 3)
var tabuleiroModificado = tabuleiro
revelarCelula(tabuleiro: &tabuleiroModificado, linha: 2, coluna: 3)
```

Neste código, começamos definindo a estrutura `Celula`, que representa uma célula do tabuleiro. Cada célula possui um valor (representando a quantidade de minas adjacentes), um estado de revelação e um indicador de mina.

Em seguida, temos a função `gerarTabuleiro`, que recebe o número de linhas e colunas desejadas e retorna um tabuleiro aleatório com as minas distribuídas e os valores das células calculados.

A função `exibirTabuleiro` recebe um tabuleiro e imprime sua representação na tela. Células reveladas são mostradas com seus valores ou com o indicador de mina, e células não reveladas são representadas por um hífen.

A função `revelarCelula` recebe um tabuleiro, uma linha e uma coluna como parâmetros. Essa função revela a célula na posição especificada e verifica se o jogador ganhou ou perdeu o jogo. Se a célula revelada contém uma mina, o jogador perde. Se todas as células não minadas foram reveladas, o jogador ganha. Caso contrário, a função continua revelando células vazias recursivamente.

Por fim, temos um exemplo de uso do código. Primeiro, geramos um tabuleiro 5x5. Em seguida, exibimos o tabuleiro inicialmente oculto. Depois, revelamos uma célula (linha 2, coluna 3) e o tabuleiro é atualizado e exibido novamente.

Espero que este código atenda às suas expectativas!