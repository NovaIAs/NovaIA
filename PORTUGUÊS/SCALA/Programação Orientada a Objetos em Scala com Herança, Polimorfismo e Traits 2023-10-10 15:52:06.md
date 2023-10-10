Claro! Abaixo está um exemplo de código em Scala que cria uma classe chamada "Pessoa" e demonstra algumas funcionalidades avançadas, como herança, polimorfismo e uso de traits:

```scala
// Definindo uma trait chamada "Andar"
trait Andar {
  def andar(): Unit
}

// Definindo uma classe abstrata chamada "Animal" que estende a trait "Andar"
abstract class Animal extends Andar {
  def nome: String
  def idade: Int

  override def andar(): Unit = {
    println(s"$nome está andando...")
  }
}

// Definindo uma classe "Pessoa" que herda da classe abstrata "Animal"
class Pessoa(val nome: String, val idade: Int) extends Animal {
  override def andar(): Unit = {
    println(s"$nome está andando rapidamente...")
  }
}

// Definindo uma classe "Cachorro" que herda da classe abstrata "Animal"
class Cachorro(val nome: String, val idade: Int) extends Animal {
  override def andar(): Unit = {
    println(s"$nome está correndo...")
  }
}

// Função principal
object Main extends App {
  val pessoa = new Pessoa("João", 25)
  val cachorro = new Cachorro("Rex", 3)

  val animais: List[Animal] = List(pessoa, cachorro)

  animais.foreach(animal => {
    animal.andar()
  })
}
```

Neste exemplo, temos uma classe abstrata chamada "Animal" que define um método abstrato "andar" e duas propriedades: "nome" e "idade". Essa classe é estendida pela classe "Pessoa" e pela classe "Cachorro", que implementam o método "andar" de forma diferente.

Também temos uma trait chamada "Andar" que define o método "andar". A classe "Pessoa" e a classe "Cachorro" implementam essa trait, o que significa que elas têm a capacidade de andar.

Na função principal, criamos uma instância de "Pessoa" e uma instância de "Cachorro". Em seguida, criamos uma lista de animais que contém essas instâncias.

Por fim, utilizamos o método "foreach" para percorrer a lista de animais e chamamos o método "andar" para cada animal. Dependendo do tipo de animal, o comportamento do método "andar" será diferente.

Espero que este exemplo complexo em Scala tenha atendido às suas expectativas!