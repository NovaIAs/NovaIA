Claro! Aqui está um código complexo em Groovy que realiza uma análise de dados e gera um relatório personalizado. 

```groovy
import groovy.json.JsonSlurper

// Função para ler o arquivo JSON
def lerArquivoJson(caminhoArquivo) {
    def jsonSlurper = new JsonSlurper()
    def arquivoJson = new File(caminhoArquivo)
    
    if (arquivoJson.exists()) {
        return jsonSlurper.parse(arquivoJson)
    } else {
        throw new FileNotFoundException("Arquivo não encontrado: ${caminhoArquivo}")
    }
}

// Função para processar os dados e gerar o relatório
def gerarRelatorio(dados) {
    def relatorio = ""
    
    // Análise dos dados
    def totalRegistros = dados.size()
    def somaValores = dados.sum { it.valor }
    def mediaValores = dados.sum { it.valor } / totalRegistros
    def valorMinimo = dados.min { it.valor }.valor
    def valorMaximo = dados.max { it.valor }.valor
    
    // Construção do relatório
    relatorio += "Relatório de Dados:\n"
    relatorio += "Total de registros: ${totalRegistros}\n"
    relatorio += "Soma dos valores: ${somaValores}\n"
    relatorio += "Média dos valores: ${mediaValores}\n"
    relatorio += "Valor mínimo: ${valorMinimo}\n"
    relatorio += "Valor máximo: ${valorMaximo}\n"
    
    return relatorio
}

// Caminho do arquivo JSON
def caminhoArquivoJson = "/caminho/do/arquivo.json"

try {
    // Leitura do arquivo JSON
    def dadosJson = lerArquivoJson(caminhoArquivoJson)
    
    // Processamento dos dados e geração do relatório
    def relatorioFinal = gerarRelatorio(dadosJson)
    
    // Impressão do relatório
    println(relatorioFinal)
} catch (FileNotFoundException e) {
    println(e.getMessage())
}
```

Explicação do código:

1. Primeiro, importamos a classe `JsonSlurper` do Groovy, que nos permite ler e analisar arquivos JSON.
2. Em seguida, definimos a função `lerArquivoJson` que recebe o caminho do arquivo JSON como parâmetro e retorna o conteúdo do arquivo convertido em um objeto Groovy.
3. Após isso, definimos a função `gerarRelatorio` que recebe os dados como parâmetro e realiza uma análise estatística desses dados, calculando o total de registros, a soma dos valores, a média dos valores, o valor mínimo e o valor máximo.
4. Dentro da função `gerarRelatorio`, é construída uma string contendo o relatório, utilizando os valores calculados na análise.
5. No corpo principal do código, é definido o caminho do arquivo JSON a ser lido.
6. Em um bloco `try-catch`, é realizada a leitura do arquivo JSON chamando a função `lerArquivoJson`.
7. Em seguida, os dados são passados para a função `gerarRelatorio` e o resultado é armazenado em `relatorioFinal`.
8. Por fim, o relatório é impresso no console.

Espero que este código atenda às suas expectativas!