Certamente! Abaixo está um exemplo de um código complexo em Scala, que faz uso de várias funcionalidades da linguagem de programação. O código cria uma classe chamada `ManipuladorArquivos` que possui métodos para manipulação de arquivos. 

```scala
import java.io.{File, PrintWriter}

class ManipuladorArquivos(diretorio: String) {

  def criarDiretorio(): File = {
    val diretorioFile = new File(diretorio)
    if (!diretorioFile.exists()) {
      diretorioFile.mkdirs()
    }
    diretorioFile
  }

  def criarArquivo(nome: String, conteudo: String): File = {
    val diretorioFile = criarDiretorio()
    val arquivo = new File(diretorioFile, nome)
    val writer = new PrintWriter(arquivo)
    writer.write(conteudo)
    writer.close()
    arquivo
  }

  def listarArquivos(): List[File] = {
    val diretorioFile = criarDiretorio()
    val arquivos = diretorioFile.listFiles()
    if (arquivos != null) {
      arquivos.toList
    } else {
      Nil
    }
  }

  def apagarArquivo(nome: String): Unit = {
    val diretorioFile = criarDiretorio()
    val arquivo = new File(diretorioFile, nome)
    if (arquivo.exists()) {
      arquivo.delete()
    }
  }
}

object Main extends App {
  val manipulador = new ManipuladorArquivos("diretorio_exemplo")

  manipulador.criarDiretorio() // Cria o diretório "diretorio_exemplo" se não existir

  manipulador.criarArquivo("arquivo1.txt", "Conteúdo do arquivo 1") // Cria o arquivo "arquivo1.txt" com o conteúdo especificado

  manipulador.criarArquivo("arquivo2.txt", "Conteúdo do arquivo 2") // Cria o arquivo "arquivo2.txt" com o conteúdo especificado

  val arquivos = manipulador.listarArquivos() // Lista todos os arquivos no diretório "diretorio_exemplo"
  arquivos.foreach(arquivo => println(arquivo.getName)) // Imprime o nome de cada arquivo

  manipulador.apagarArquivo("arquivo1.txt") // Apaga o arquivo "arquivo1.txt"

  manipulador.listarArquivos().foreach(arquivo => println(arquivo.getName)) // Imprime o nome dos arquivos restantes
}
```

Explicação do código: 

- Primeiramente, importamos a classe `File` e `PrintWriter` para manipulação de arquivos.
- Em seguida, definimos a classe `ManipuladorArquivos` que recebe o diretório como parâmetro no seu construtor.
- O método `criarDiretorio()` cria um diretório com o nome especificado, caso ele não exista.
- O método `criarArquivo(nome: String, conteudo: String)` cria um arquivo dentro do diretório especificado, com o nome e conteúdo recebidos como parâmetros. O método também retorna o objeto `File` correspondente ao arquivo criado.
- O método `listarArquivos()` retorna uma lista de objetos `File` correspondentes aos arquivos presentes no diretório.
- O método `apagarArquivo(nome: String)` apaga um arquivo com o nome especificado, caso ele exista.
- No objeto `Main`, instanciamos a classe `ManipuladorArquivos` com o diretório `"diretorio_exemplo"`.
- Chamamos o método `criarDiretorio()` para garantir que o diretório existe.
- Utilizamos o método `criarArquivo()` para criar dois arquivos com conteúdos diferentes.
- Chamamos o método `listarArquivos()` para listar os arquivos presentes no diretório e imprimimos seus nomes.
- Utilizamos o método `apagarArquivo()` para apagar o arquivo `"arquivo1.txt"`.
- Chamamos novamente o método `listarArquivos()` para listar os arquivos restantes e imprimimos seus nomes.

Esse exemplo demonstra o uso de orientação a objetos, tratamento de arquivos e manipulação de diretórios em Scala.