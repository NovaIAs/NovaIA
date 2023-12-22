Claro! Aqui está um código complexo em Scala, que utiliza diversas funcionalidades da linguagem e é explicado linha por linha:

```scala
// Importando bibliotecas necessárias
import scala.collection.mutable.{ListBuffer, Map}

// Definindo uma classe Pessoa
class Pessoa(val nome: String, val idade: Int)

object Main {
  def main(args: Array[String]): Unit = {

    // Criando uma lista de pessoas
    val pessoas = ListBuffer(
      new Pessoa("João", 25),
      new Pessoa("Maria", 30),
      new Pessoa("Pedro", 20),
      new Pessoa("Ana", 35)
    )

    // Filtrando pessoas com idade acima de 25 anos
    val pessoasAcimaDe25 = pessoas.filter(_.idade > 25)

    // Criando um mapa para armazenar a contagem de pessoas por idade
    val contagemPorIdade = Map[Int, Int]()

    // Contando a quantidade de pessoas por idade
    for (pessoa <- pessoas) {
      if (contagemPorIdade.contains(pessoa.idade)) {
        contagemPorIdade(pessoa.idade) += 1
      } else {
        contagemPorIdade(pessoa.idade) = 1
      }
    }

    // Imprimindo a quantidade de pessoas por idade
    println("Contagem de pessoas por idade:")
    for ((idade, quantidade) <- contagemPorIdade) {
      println(s"Idade: $idade - Quantidade: $quantidade")
    }

    // Ordenando a lista de pessoas por idade em ordem decrescente
    val pessoasOrdenadas = pessoas.sortBy(_.idade)(Ordering[Int].reverse)

    // Imprimindo a lista de pessoas ordenada
    println("Lista de pessoas ordenada por idade:")
    for (pessoa <- pessoasOrdenadas) {
      println(s"Nome: ${pessoa.nome} - Idade: ${pessoa.idade}")
    }
  }
}
```

Neste código, começamos importando as bibliotecas necessárias. Em seguida, definimos uma classe `Pessoa` com os atributos `nome` e `idade`.

Dentro do objeto `Main`, temos a função `main` que é o ponto de entrada do programa. Aqui, criamos uma lista de objetos `Pessoa` chamada `pessoas`, com alguns exemplos de pessoas.

Em seguida, filtramos as pessoas que possuem idade acima de 25 anos utilizando a função `filter`.

Criamos um mapa chamado `contagemPorIdade` para armazenar a contagem de pessoas por idade. Percorremos a lista `pessoas` e, para cada pessoa, verificamos se a idade dela já está presente no mapa `contagemPorIdade`. Se estiver, incrementamos a contagem. Caso contrário, adicionamos a idade no mapa com contagem igual a 1.

Imprimimos a quantidade de pessoas por idade utilizando um loop `for` e a notação de desestruturação `(idade, quantidade) <- contagemPorIdade`.

Em seguida, ordenamos a lista de pessoas por idade em ordem decrescente utilizando a função `sortBy` e imprimimos a lista ordenada.

Espero que este código atenda às suas expectativas!