Claro, vou criar um código complexo em Scala para você. Este código será uma implementação de um algoritmo de árvore binária de busca balanceada, conhecida como árvore AVL. 

Uma árvore AVL é uma estrutura de dados que se assemelha a uma árvore binária de busca, mas com a diferença de que ela mantém altura balanceada em todos os seus nós. Isso significa que a diferença de altura entre as subárvores esquerda e direita de qualquer nó é no máximo 1. Dessa forma, a árvore AVL garante um tempo de busca, inserção e remoção eficiente, mantendo-se sempre balanceada.

Aqui está o código em Scala:

```scala
// Definição da classe Node que representa um nó da árvore
class Node(var value: Int, var left: Option[Node], var right: Option[Node], var height: Int)

// Definição da classe AVLTree que representa a árvore AVL
class AVLTree {
  var root: Option[Node] = None

  // Função para obter a altura de um nó
  private def height(node: Option[Node]): Int = node match {
    case Some(n) => n.height
    case None => -1
  }

  // Função para atualizar a altura de um nó
  private def updateHeight(node: Option[Node]): Unit = node match {
    case Some(n) => n.height = 1 + Math.max(height(n.left), height(n.right))
    case None =>
  }

  // Função para obter o fator de balanceamento de um nó
  private def balanceFactor(node: Option[Node]): Int = node match {
    case Some(n) => height(n.left) - height(n.right)
    case None => 0
  }

  // Função para fazer uma rotação simples à esquerda
  private def rotateLeft(node: Option[Node]): Option[Node] = node match {
    case Some(n) =>
      val newRoot = n.right.get
      n.right = newRoot.left
      newRoot.left = Some(n)
      updateHeight(n)
      updateHeight(newRoot)
      Some(newRoot)
    case None => None
  }

  // Função para fazer uma rotação simples à direita
  private def rotateRight(node: Option[Node]): Option[Node] = node match {
    case Some(n) =>
      val newRoot = n.left.get
      n.left = newRoot.right
      newRoot.right = Some(n)
      updateHeight(n)
      updateHeight(newRoot)
      Some(newRoot)
    case None => None
  }

  // Função para fazer uma rotação dupla à esquerda
  private def rotateDoubleLeft(node: Option[Node]): Option[Node] = node match {
    case Some(n) =>
      n.right = rotateRight(n.right)
      rotateLeft(node)
    case None => None
  }

  // Função para fazer uma rotação dupla à direita
  private def rotateDoubleRight(node: Option[Node]): Option[Node] = node match {
    case Some(n) =>
      n.left = rotateLeft(n.left)
      rotateRight(node)
    case None => None
  }

  // Função para inserir um valor na árvore AVL
  def insert(value: Int): Unit = {
    def insertNode(node: Option[Node]): Option[Node] = node match {
      case Some(n) =>
        if (value < n.value) {
          n.left = insertNode(n.left)
          if (balanceFactor(n) == 2) {
            if (value < n.left.get.value) {
              rotateRight(node)
            } else {
              rotateDoubleRight(node)
            }
          }
        } else if (value > n.value) {
          n.right = insertNode(n.right)
          if (balanceFactor(n) == -2) {
            if (value > n.right.get.value) {
              rotateLeft(node)
            } else {
              rotateDoubleLeft(node)
            }
          }
        }
        updateHeight(n)
        Some(n)
      case None => Some(new Node(value, None, None, 0))
    }

    root = insertNode(root)
  }

  // Função para imprimir a árvore AVL em ordem
  def printInOrder(): Unit = {
    def inOrder(node: Option[Node]): Unit = node match {
      case Some(n) =>
        inOrder(n.left)
        println(n.value)
        inOrder(n.right)
      case None =>
    }

    inOrder(root)
  }
}

// Exemplo de uso da árvore AVL
val tree = new AVLTree()
tree.insert(10)
tree.insert(5)
tree.insert(15)
tree.insert(3)
tree.insert(7)
tree.insert(12)
tree.insert(17)

tree.printInOrder()
```

Neste código, temos as classes `Node` e `AVLTree`. A classe `Node` representa um nó da árvore com um valor, referências para os nós esquerdo e direito, e a altura do nó. A classe `AVLTree` representa a árvore AVL propriamente dita, com um nó raiz e métodos para inserção e impressão em ordem.

A inserção na árvore AVL é feita de forma recursiva, mantendo o balanceamento da árvore através das rotações simples e duplas, dependendo do fator de balanceamento do nó. O método `printInOrder` imprime os valores da árvore em ordem crescente.

Este código é apenas um exemplo complexo de implementação de uma árvore AVL em Scala. Existem muitas outras implementações possíveis e variações deste algoritmo.