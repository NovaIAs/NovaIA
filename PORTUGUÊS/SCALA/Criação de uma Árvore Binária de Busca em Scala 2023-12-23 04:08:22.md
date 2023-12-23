Claro! Aqui está um código complexo em Scala que envolve o uso de classes, métodos e recursão para criar uma árvore binária de busca e realizar algumas operações nela. Vou explicar o código passo a passo.

```scala
// Classe que representa um nó na árvore binária de busca
class No(var valor: Int, var esquerda: Option[No], var direita: Option[No]) {
  // Método para inserir um valor na árvore
  def inserir(valorNovo: Int): Unit = {
    if (valorNovo < valor) {
      if (esquerda.isEmpty) {
        esquerda = Some(new No(valorNovo, None, None))
      } else {
        esquerda.get.inserir(valorNovo)
      }
    } else if (valorNovo > valor) {
      if (direita.isEmpty) {
        direita = Some(new No(valorNovo, None, None))
      } else {
        direita.get.inserir(valorNovo)
      }
    }
  }

  // Método para verificar se um valor existe na árvore
  def buscar(valorAlvo: Int): Boolean = {
    if (valorAlvo == valor) {
      true
    } else if (valorAlvo < valor) {
      esquerda.exists(_.buscar(valorAlvo))
    } else {
      direita.exists(_.buscar(valorAlvo))
    }
  }

  // Método para imprimir a árvore em ordem
  def imprimir(): Unit = {
    esquerda.foreach(_.imprimir())
    print(valor + " ")
    direita.foreach(_.imprimir())
  }
}

// Classe que representa a árvore binária de busca
class ArvoreBinaria {
  private var raiz: Option[No] = None

  // Método para inserir um valor na árvore
  def inserir(valor: Int): Unit = {
    if (raiz.isEmpty) {
      raiz = Some(new No(valor, None, None))
    } else {
      raiz.get.inserir(valor)
    }
  }

  // Método para verificar se um valor existe na árvore
  def buscar(valor: Int): Boolean = {
    raiz.exists(_.buscar(valor))
  }

  // Método para imprimir a árvore em ordem
  def imprimir(): Unit = {
    raiz.foreach(_.imprimir())
    println()
  }
}

// Exemplo de uso da árvore binária de busca
object Main extends App {
  val arvore = new ArvoreBinaria()

  arvore.inserir(10)
  arvore.inserir(5)
  arvore.inserir(15)
  arvore.inserir(3)
  arvore.inserir(7)
  arvore.inserir(12)
  arvore.inserir(20)

  println("Impressão da árvore em ordem:")
  arvore.imprimir()

  println("Buscar o valor 7 na árvore: " + arvore.buscar(7))
  println("Buscar o valor 18 na árvore: " + arvore.buscar(18))
}
```

Neste código, temos duas classes principais: `No` e `ArvoreBinaria`. A classe `No` representa um nó na árvore binária de busca e possui os atributos `valor`, `esquerda` e `direita`. O método `inserir` é responsável por inserir um valor na árvore seguindo a lógica da árvore binária de busca. O método `buscar` verifica se um determinado valor existe na árvore. O método `imprimir` imprime a árvore em ordem.

A classe `ArvoreBinaria` é responsável por encapsular a raiz da árvore e fornece métodos para inserir um valor, buscar um valor e imprimir a árvore.

No exemplo de uso, criamos uma instância da classe `ArvoreBinaria` e inserimos alguns valores na árvore. Em seguida, imprimimos a árvore em ordem e realizamos duas buscas, uma para o valor 7 (que existe na árvore) e outra para o valor 18 (que não existe na árvore).

Espero que esse código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.