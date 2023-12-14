Claro! Aqui está um código complexo em Groovy que envolve manipulação de arquivos, criação de classes e uso de threads:

```groovy
import java.io.File
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class ProcessadorArquivo implements Runnable {
    File arquivo

    ProcessadorArquivo(File arquivo) {
        this.arquivo = arquivo
    }

    void run() {
        def conteudo = arquivo.text
        def palavras = conteudo.split("\\s+")
        def contagem = [:]

        for (palavra in palavras) {
            contagem[palavra] = contagem.getOrDefault(palavra, 0) + 1
        }

        println("Contagem de palavras no arquivo ${arquivo.name}:")
        for (palavra, quantidade in contagem) {
            println("${palavra}: ${quantidade}")
        }
    }
}

void processarDiretorio(File diretorio) {
    def arquivos = diretorio.listFiles()
    def pool = Executors.newFixedThreadPool(Runtime.runtime.availableProcessors())

    for (arquivo in arquivos) {
        if (arquivo.isFile() && arquivo.name.endsWith(".txt")) {
            pool.execute(new ProcessadorArquivo(arquivo))
        } else if (arquivo.isDirectory()) {
            processarDiretorio(arquivo)
        }
    }

    pool.shutdown()
}

def diretorioRaiz = new File("caminho/do/diretorio/raiz")
processarDiretorio(diretorioRaiz)
```

Este código em Groovy irá percorrer um diretório raiz e processar todos os arquivos de texto encontrados. Para cada arquivo, ele irá criar uma thread usando a classe `ProcessadorArquivo`. Cada thread irá ler o conteúdo do arquivo, contar a quantidade de ocorrências de cada palavra e imprimir a contagem no console.

A classe `ProcessadorArquivo` implementa a interface `Runnable`, o que permite que ela seja executada em uma thread separada. No método `run()`, o conteúdo do arquivo é lido e dividido em palavras usando o método `split()`. Em seguida, um mapa chamado `contagem` é usado para contar a quantidade de ocorrências de cada palavra. Por fim, a contagem é impressa no console.

O método `processarDiretorio` é responsável por percorrer recursivamente o diretório e seus subdiretórios em busca de arquivos de texto. Para cada arquivo encontrado, ele verifica se é um arquivo de texto (usando o método `endsWith()`) e cria uma nova thread para processá-lo. Se for um diretório, ele chama recursivamente o método `processarDiretorio`.

No final, o código cria um objeto `File` com o caminho do diretório raiz e chama o método `processarDiretorio` para iniciar o processamento.

Espero que este exemplo atenda às suas expectativas!