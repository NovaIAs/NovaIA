Claro! Aqui está um exemplo de código complexo em Groovy que realiza a multiplicação de matrizes:

```groovy
def matrizA = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
def matrizB = [[9, 8, 7], [6, 5, 4], [3, 2, 1]]

def multiplicarMatrizes(matrizA, matrizB) {
    def linhasA = matrizA.size()
    def colunasA = matrizA[0].size()
    def colunasB = matrizB[0].size()
    
    if (colunasA != matrizB.size()) {
        throw new IllegalArgumentException("As matrizes não podem ser multiplicadas")
    }
    
    def matrizResultado = []
    
    for (int i = 0; i < linhasA; i++) {
        matrizResultado << []
        for (int j = 0; j < colunasB; j++) {
            def soma = 0
            for (int k = 0; k < colunasA; k++) {
                soma += matrizA[i][k] * matrizB[k][j]
            }
            matrizResultado[i] << soma
        }
    }
    
    matrizResultado
}

def resultado = multiplicarMatrizes(matrizA, matrizB)
println "Resultado da multiplicação das matrizes:"
println resultado
```

Neste código, temos uma função `multiplicarMatrizes` que recebe duas matrizes como parâmetros. Primeiro, verificamos se as dimensões das matrizes são compatíveis para a multiplicação. Caso não sejam, lançamos uma exceção.

Em seguida, criamos a matriz de resultado vazia. Utilizando três loops aninhados, percorremos as linhas da matrizA, as colunas da matrizB e os elementos das colunas da matrizA e linhas da matrizB. Multiplicamos os elementos correspondentes e somamos para obter o valor do elemento da matriz resultado.

Por fim, imprimimos o resultado da multiplicação das matrizes.

Espero que este exemplo atenda às suas expectativas!