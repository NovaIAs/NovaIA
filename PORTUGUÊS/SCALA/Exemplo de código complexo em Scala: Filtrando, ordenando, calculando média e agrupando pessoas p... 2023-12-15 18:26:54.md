Claro! Aqui está um exemplo de código complexo em Scala:

```scala
// Classe para representar uma pessoa
case class Pessoa(nome: String, idade: Int)

object Main extends App {
  
  // Lista de pessoas
  val pessoas = List(
    Pessoa("João", 25),
    Pessoa("Maria", 30),
    Pessoa("Pedro", 40),
    Pessoa("Ana", 22),
    Pessoa("José", 35),
    Pessoa("Mariana", 27),
    Pessoa("Carlos", 50),
    Pessoa("Lúcia", 33),
    Pessoa("Rafael", 29),
    Pessoa("Laura", 45)
  )
  
  // Filtrar pessoas com idade acima de 30 anos
  val pessoasAcima30 = pessoas.filter(_.idade > 30)
  println("Pessoas acima de 30 anos:")
  pessoasAcima30.foreach(p => println(s"${p.nome}, ${p.idade} anos"))
  
  // Ordenar pessoas por idade (crescente)
  val pessoasOrdenadas = pessoas.sortBy(_.idade)
  println("\nPessoas ordenadas por idade:")
  pessoasOrdenadas.foreach(p => println(s"${p.nome}, ${p.idade} anos"))
  
  // Calcular a média de idade das pessoas
  val mediaIdade = pessoas.map(_.idade).sum.toDouble / pessoas.length
  println(s"\nMédia de idade: $mediaIdade anos")
  
  // Agrupar pessoas por faixa etária
  val faixasEtarias = Map(
    "18-25" -> (18 to 25),
    "26-35" -> (26 to 35),
    "36-45" -> (36 to 45),
    "46+" -> (46 to Int.MaxValue)
  )
  
  val pessoasPorFaixaEtaria = pessoas.groupBy { pessoa =>
    faixasEtarias.find { case (_, faixa) => faixa.contains(pessoa.idade) }
      .map { case (faixaEtaria, _) => faixaEtaria }
      .getOrElse("Desconhecida")
  }
  
  println("\nPessoas por faixa etária:")
  pessoasPorFaixaEtaria.foreach { case (faixaEtaria, pessoas) =>
    println(s"$faixaEtaria: ${pessoas.map(_.nome).mkString(", ")}")
  }
}
```

Neste código, temos uma classe `Pessoa` que possui os atributos `nome` e `idade`. Em seguida, temos um objeto `Main` que contém o ponto de entrada do programa.

No exemplo, temos uma lista de pessoas com diferentes nomes e idades. Usamos a função `filter` para filtrar as pessoas com idade acima de 30 anos e exibimos o resultado na tela.

Em seguida, utilizamos a função `sortBy` para ordenar as pessoas por idade em ordem crescente e também exibimos o resultado.

Calculamos a média de idade das pessoas usando a função `map` para extrair as idades, `sum` para somar as idades e `length` para obter o tamanho da lista.

Por fim, agrupamos as pessoas por faixa etária usando um mapa `faixasEtarias` que associa cada faixa etária a um intervalo de idades. Utilizamos a função `groupBy` para agrupar as pessoas de acordo com a sua faixa etária e exibimos o resultado na tela.

Espero que esse código complexo em Scala atenda às suas expectativas! Se tiver alguma dúvida, estou à disposição para ajudar.