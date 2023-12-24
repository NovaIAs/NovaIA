Claro, vou criar um código complexo em Groovy para você. Aqui está um exemplo de um código que lê um arquivo de entrada, realiza várias operações matemáticas e grava o resultado em um arquivo de saída:

```groovy
import java.io.File

def lerArquivo(String nomeArquivo) {
    def arquivo = new File(nomeArquivo)
    def linhas = arquivo.readLines()
    return linhas
}

def realizarOperacoes(List<String> linhas) {
    def resultado = 0

    for (linha in linhas) {
        def operacao = linha.split(" ")

        switch (operacao[0]) {
            case "soma":
                resultado += operacao[1].toInteger()
                break
            case "subtracao":
                resultado -= operacao[1].toInteger()
                break
            case "multiplicacao":
                resultado *= operacao[1].toInteger()
                break
            case "divisao":
                resultado /= operacao[1].toInteger()
                break
            case "potencia":
                resultado = Math.pow(resultado, operacao[1].toInteger()).toInteger()
                break
            default:
                println "Operação inválida: ${operacao[0]}"
        }
    }
    
    return resultado
}

def gravarArquivo(String nomeArquivo, int resultado) {
    def arquivo = new File(nomeArquivo)
    arquivo.write(resultado.toString())
    println "Resultado gravado no arquivo '${nomeArquivo}'"
}

def executarCodigo(String arquivoEntrada, String arquivoSaida) {
    def linhas = lerArquivo(arquivoEntrada)
    def resultado = realizarOperacoes(linhas)
    gravarArquivo(arquivoSaida, resultado)
}

executarCodigo("entrada.txt", "saida.txt")
```

Neste código, temos três funções principais:

1. `lerArquivo`: Essa função recebe o nome de um arquivo como parâmetro e lê todas as linhas do arquivo. Retorna uma lista de strings, onde cada string representa uma linha do arquivo.

2. `realizarOperacoes`: Essa função recebe a lista de linhas lidas do arquivo e executa as operações matemáticas definidas em cada linha. O resultado é retornado como um número inteiro.

3. `gravarArquivo`: Essa função recebe o nome de um arquivo de saída e o resultado das operações. Ela grava o resultado no arquivo.

Por fim, a função `executarCodigo` chama as três funções anteriores, passando os nomes dos arquivos de entrada e saída como parâmetros.

Para executar esse código, você precisa criar um arquivo de entrada chamado "entrada.txt" com as operações desejadas, onde cada linha segue o seguinte formato: `operacao valor`. Por exemplo:

```
soma 5
subtracao 2
multiplicacao 3
divisao 2
potencia 3
```

O resultado das operações será gravado no arquivo de saída "saida.txt".