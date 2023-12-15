Claro! Abaixo está um exemplo de código complexo em Groovy, que lida com manipulação de arquivos e processamento de dados:

```groovy
import groovy.json.JsonSlurper

// Função para ler um arquivo JSON e retornar um objeto
def lerArquivoJson(arquivo) {
    def jsonSlurper = new JsonSlurper()
    def json = jsonSlurper.parseText(new File(arquivo).text)
    return json
}

// Função para filtrar os dados de um objeto JSON
def filtrarDados(json, chave, valor) {
    def resultado = []
    json.each { item ->
        if (item[chave] == valor) {
            resultado.add(item)
        }
    }
    return resultado
}

// Função para processar os dados filtrados
def processarDados(dadosFiltrados) {
    dadosFiltrados.each { item ->
        println "Item encontrado: ${item}"
        // Realizar outras operações ou processamentos aqui
    }
}

// Caminho do arquivo JSON
def caminhoArquivo = "caminho/do/arquivo.json"

// Ler o arquivo JSON
def dadosJson = lerArquivoJson(caminhoArquivo)

// Filtrar os dados baseado em uma chave e valor
def dadosFiltrados = filtrarDados(dadosJson, "chave", "valor")

// Processar os dados filtrados
processarDados(dadosFiltrados)
```

Neste exemplo, estamos lendo um arquivo JSON, filtrando os dados com base em uma chave e valor específicos e, em seguida, processando esses dados de acordo com as necessidades do programa.

A função `lerArquivoJson` recebe o caminho de um arquivo JSON, utiliza a biblioteca `JsonSlurper` para fazer o parsing do arquivo e retorna um objeto JSON.

A função `filtrarDados` recebe o objeto JSON, uma chave e um valor, e filtra os dados do JSON com base nessa chave e valor, retornando um array com os itens correspondentes.

A função `processarDados` recebe o array de dados filtrados e realiza o processamento necessário para cada item.

No exemplo final, definimos o caminho do arquivo JSON, chamamos as funções necessárias e processamos os dados filtrados.