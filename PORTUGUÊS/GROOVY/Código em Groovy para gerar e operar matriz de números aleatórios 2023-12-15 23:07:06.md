Claro! Aqui está um código complexo em Groovy que gera uma matriz de números aleatórios e realiza operações matemáticas com eles:

```groovy
import java.util.Random

// Método para gerar uma matriz de números aleatórios
def gerarMatriz(dimensao) {
    def matriz = []
    def random = new Random()

    for (int i = 0; i < dimensao; i++) {
        def linha = []
        for (int j = 0; j < dimensao; j++) {
            linha.add(random.nextInt(100))
        }
        matriz.add(linha)
    }

    return matriz
}

// Método para calcular a soma dos elementos da matriz
def calcularSoma(matriz) {
    def soma = 0

    for (int i = 0; i < matriz.size(); i++) {
        for (int j = 0; j < matriz[i].size(); j++) {
            soma += matriz[i][j]
        }
    }

    return soma
}

// Método para calcular a média dos elementos da matriz
def calcularMedia(matriz) {
    def soma = calcularSoma(matriz)
    def media = soma / (matriz.size() * matriz[0].size())

    return media
}

// Método para calcular o produto dos elementos da matriz
def calcularProduto(matriz) {
    def produto = 1

    for (int i = 0; i < matriz.size(); i++) {
        for (int j = 0; j < matriz[i].size(); j++) {
            produto *= matriz[i][j]
        }
    }

    return produto
}

// Método para exibir a matriz
def exibirMatriz(matriz) {
    for (int i = 0; i < matriz.size(); i++) {
        for (int j = 0; j < matriz[i].size(); j++) {
            print(matriz[i][j] + "\t")
        }
        println()
    }
}

// Dimensão da matriz
def dimensaoMatriz = 5

// Gerar matriz
def matriz = gerarMatriz(dimensaoMatriz)

// Exibir matriz
println("Matriz gerada:")
exibirMatriz(matriz)

// Calcular e exibir a soma dos elementos da matriz
def soma = calcularSoma(matriz)
println("\nSoma dos elementos da matriz: $soma")

// Calcular e exibir a média dos elementos da matriz
def media = calcularMedia(matriz)
println("Média dos elementos da matriz: $media")

// Calcular e exibir o produto dos elementos da matriz
def produto = calcularProduto(matriz)
println("Produto dos elementos da matriz: $produto")
```

Explicação:

1. Começamos importando a classe `Random` para gerar números aleatórios.
2. Definimos a função `gerarMatriz` que recebe como parâmetro a dimensão da matriz e retorna uma matriz preenchida com números aleatórios de 0 a 100.
3. Dentro do método `gerarMatriz`, utilizamos um laço `for` para criar uma lista de listas que representa a matriz. Em cada iteração, geramos um número aleatório e adicionamos à linha da matriz.
4. A função `calcularSoma` recebe a matriz como parâmetro e retorna a soma de todos os elementos.
5. Também utilizamos um laço `for` dentro de `calcularSoma` para percorrer todos os elementos da matriz e somá-los.
6. A função `calcularMedia` recebe a matriz como parâmetro, chama a função `calcularSoma` para obter a soma e retorna a média dos elementos.
7. A função `calcularProduto` recebe a matriz como parâmetro e retorna o produto de todos os elementos.
8. Assim como em `calcularSoma`, utilizamos um laço `for` para percorrer todos os elementos da matriz e multiplicá-los.
9. A função `exibirMatriz` recebe a matriz como parâmetro e exibe-a na tela.
10. A seguir, definimos a dimensão da matriz e geramos uma matriz aleatória chamando a função `gerarMatriz`.
11. Exibimos a matriz gerada utilizando a função `exibirMatriz`.
12. Calculamos a soma, média e produto dos elementos da matriz chamando as respectivas funções e exibimos os resultados na tela.