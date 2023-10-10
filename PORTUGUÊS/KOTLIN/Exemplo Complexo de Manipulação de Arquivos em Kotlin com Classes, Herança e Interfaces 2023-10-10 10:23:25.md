Claro! Aqui está um exemplo de um código complexo em Kotlin que envolve o uso de classes, herança, interfaces e exceções:

```kotlin
import java.io.File

// Definindo uma interface para manipulação de arquivos
interface ArquivoManipulavel {
    fun abrirArquivo(nomeArquivo: String)
    fun salvarArquivo()
}

// Classe base para manipulação de arquivos
open class ManipuladorArquivo : ArquivoManipulavel {
    protected var arquivo: File? = null

    override fun abrirArquivo(nomeArquivo: String) {
        arquivo = File(nomeArquivo)
        println("Arquivo $nomeArquivo aberto.")
    }

    override fun salvarArquivo() {
        if (arquivo != null) {
            println("Arquivo ${arquivo!!.name} salvo.")
        } else {
            throw ArquivoNaoEncontradoException("Nenhum arquivo aberto.")
        }
    }
}

// Classe de exceção personalizada para arquivo não encontrado
class ArquivoNaoEncontradoException(message: String) : Exception(message)

// Classe para manipulação de arquivos de texto
class ManipuladorArquivoTexto : ManipuladorArquivo() {
    private var conteudo: String = ""

    fun lerArquivo() {
        if (arquivo != null) {
            conteudo = arquivo!!.readText()
            println("Conteúdo do arquivo lido: $conteudo")
        } else {
            throw ArquivoNaoEncontradoException("Nenhum arquivo aberto.")
        }
    }

    fun adicionarTexto(texto: String) {
        conteudo += texto
        println("Texto adicionado ao conteúdo do arquivo.")
    }
}

// Classe para manipulação de arquivos de imagem
class ManipuladorArquivoImagem : ManipuladorArquivo() {
    private var imagem: ByteArray? = null

    fun carregarImagem() {
        if (arquivo != null) {
            imagem = arquivo!!.readBytes()
            println("Imagem carregada.")
        } else {
            throw ArquivoNaoEncontradoException("Nenhum arquivo aberto.")
        }
    }

    fun redimensionarImagem(largura: Int, altura: Int) {
        if (imagem != null) {
            // Lógica para redimensionar a imagem
            println("Imagem redimensionada para $largura x $altura.")
        } else {
            throw ArquivoNaoEncontradoException("Nenhuma imagem carregada.")
        }
    }
}

fun main() {
    try {
        val manipuladorTexto = ManipuladorArquivoTexto()
        manipuladorTexto.abrirArquivo("arquivo.txt")
        manipuladorTexto.lerArquivo()
        manipuladorTexto.adicionarTexto("Novo texto")

        val manipuladorImagem = ManipuladorArquivoImagem()
        manipuladorImagem.abrirArquivo("imagem.jpg")
        manipuladorImagem.carregarImagem()
        manipuladorImagem.redimensionarImagem(800, 600)

        manipuladorTexto.salvarArquivo()
        manipuladorImagem.salvarArquivo()
    } catch (ex: ArquivoNaoEncontradoException) {
        println("Erro: ${ex.message}")
    }
}
```

Neste código em Kotlin, temos a criação de três classes: `ManipuladorArquivo`, `ManipuladorArquivoTexto` e `ManipuladorArquivoImagem`. A classe `ManipuladorArquivo` é uma classe base abstrata que implementa a interface `ArquivoManipulavel`. Ela possui métodos para abrir e salvar arquivos, além de uma propriedade para armazenar o objeto `File` do arquivo aberto.

As classes `ManipuladorArquivoTexto` e `ManipuladorArquivoImagem` herdam da classe `ManipuladorArquivo`. Cada uma dessas classes possui métodos específicos para manipulação de arquivos de texto e imagens, respectivamente.

O método `main` é responsável por criar instâncias das classes `ManipuladorArquivoTexto` e `ManipuladorArquivoImagem`, abrir arquivos, executar ações específicas para cada tipo de arquivo e salvar os arquivos modificados.

É importante destacar que esse código é apenas um exemplo complexo, mas você pode personalizá-lo de acordo com suas necessidades específicas.