Claro! Aqui está um código complexo em Scala:

```scala
// Importando bibliotecas necessárias
import scala.collection.mutable.ListBuffer
import scala.util.Random

// Definindo a classe Pessoa
class Pessoa(val nome: String, val idade: Int, val altura: Double)

// Definindo a função principal
object Main extends App {
  // Criando uma lista vazia de pessoas
  val pessoas: ListBuffer[Pessoa] = ListBuffer()

  // Função para gerar um nome aleatório
  def gerarNome(): String = {
    val nomes = List("João", "Maria", "Pedro", "Ana", "Lucas", "Laura", "Fernanda", "Rafael")
    val sobrenomes = List("Silva", "Santos", "Costa", "Oliveira", "Pereira", "Rodrigues", "Ferreira")
    val nomeAleatorio = Random.shuffle(nomes).head
    val sobrenomeAleatorio = Random.shuffle(sobrenomes).head
    s"${nomeAleatorio} ${sobrenomeAleatorio}"
  }

  // Função para gerar uma idade aleatória entre 18 e 65 anos
  def gerarIdade(): Int = {
    val idadeAleatoria = Random.nextInt(48) + 18
    idadeAleatoria
  }

  // Função para gerar uma altura aleatória entre 1.50 e 2.00 metros
  def gerarAltura(): Double = {
    val alturaAleatoria = Random.nextDouble() * (2.00 - 1.50) + 1.50
    alturaAleatoria
  }

  // Gerando 50 pessoas aleatórias e adicionando à lista de pessoas
  for (_ <- 1 to 50) {
    val nomeAleatorio = gerarNome()
    val idadeAleatoria = gerarIdade()
    val alturaAleatoria = gerarAltura()
    val pessoa = new Pessoa(nomeAleatorio, idadeAleatoria, alturaAleatoria)
    pessoas += pessoa
  }

  // Imprimindo as informações das pessoas
  for (pessoa <- pessoas) {
    println(s"Nome: ${pessoa.nome}, Idade: ${pessoa.idade}, Altura: ${pessoa.altura}")
  }
}
```

Este é um código em Scala que gera uma lista de 50 pessoas com nomes, idades e alturas aleatórias. Aqui está uma explicação do código:

1. Começamos importando as bibliotecas necessárias `ListBuffer` e `Random`.
2. Em seguida, definimos a classe `Pessoa`, que tem os atributos `nome`, `idade` e `altura`.
3. O objeto principal `Main` é definido como `App`, o que permite executar o código diretamente.
4. Dentro do objeto `Main`, criamos uma lista vazia de pessoas usando `ListBuffer`.
5. Em seguida, definimos três funções: `gerarNome`, `gerarIdade` e `gerarAltura`. A função `gerarNome` seleciona um nome e sobrenome aleatórios de listas predefinidas. As funções `gerarIdade` e `gerarAltura` geram números aleatórios dentro de faixas específicas.
6. Usamos um loop `for` para gerar 50 pessoas aleatórias. A cada iteração do loop, criamos uma nova pessoa com nome, idade e altura aleatórios e a adicionamos à lista de pessoas.
7. Por fim, usamos outro loop `for` para imprimir as informações das pessoas, nome, idade e altura, na saída padrão.

Espero que este código atenda às suas expectativas!