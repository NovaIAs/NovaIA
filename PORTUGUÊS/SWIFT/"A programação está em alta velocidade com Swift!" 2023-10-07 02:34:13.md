Claro! Aqui está um código complexo em Swift que envolve a criação de uma estrutura de dados de uma árvore binária:

```swift
// Definição da estrutura de um nó da árvore binária
class Node<T> {
    var value: T
    var left: Node?
    var right: Node?
    
    init(value: T) {
        self.value = value
        self.left = nil
        self.right = nil
    }
}

// Definição da árvore binária
class BinaryTree<T> {
    var root: Node<T>?
    
    // Função para adicionar um novo nó na árvore
    func add(value: T) {
        let newNode = Node(value: value)
        
        // Se a árvore estiver vazia, o novo nó se torna a raiz
        if root == nil {
            root = newNode
        } else {
            addRecursive(node: root, newNode: newNode)
        }
    }
    
    // Função auxiliar recursiva para adicionar um novo nó na árvore
    private func addRecursive(node: Node<T>?, newNode: Node<T>) {
        guard let currentNode = node else {
            return
        }
        
        if newNode.value < currentNode.value {
            if currentNode.left == nil {
                currentNode.left = newNode
            } else {
                addRecursive(node: currentNode.left, newNode: newNode)
            }
        } else {
            if currentNode.right == nil {
                currentNode.right = newNode
            } else {
                addRecursive(node: currentNode.right, newNode: newNode)
            }
        }
    }
    
    // Função para percorrer a árvore em ordem (in-order traversal)
    func inOrderTraversal(node: Node<T>?) {
        guard let currentNode = node else {
            return
        }
        
        inOrderTraversal(node: currentNode.left)
        print(currentNode.value)
        inOrderTraversal(node: currentNode.right)
    }
}

// Exemplo de uso da árvore binária
let binaryTree = BinaryTree<Int>()
binaryTree.add(value: 8)
binaryTree.add(value: 4)
binaryTree.add(value: 10)
binaryTree.add(value: 2)
binaryTree.add(value: 6)
binaryTree.add(value: 12)

binaryTree.inOrderTraversal(node: binaryTree.root)
```

Nesse código em Swift, criamos uma estrutura de dados de uma árvore binária utilizando classes. A árvore binária é composta por nós, representados pela classe `Node`, que contêm um valor, um nó à esquerda e um nó à direita.

A classe `BinaryTree` representa a árvore binária em si e possui um nó raiz. A função `add` permite adicionar um novo nó à árvore, seguindo a lógica de um nó à esquerda ter um valor menor que seu nó pai, e um nó à direita ter um valor maior.

A função `inOrderTraversal` realiza um percurso em ordem (in-order traversal) na árvore, imprimindo os valores dos nós. Esse tipo de percurso visita primeiro o nó à esquerda, depois o nó atual e, por fim, o nó à direita.

No exemplo de uso apresentado, criamos uma árvore binária com valores inteiros e adicionamos alguns nós. Em seguida, realizamos um percurso em ordem (in-order traversal) na árvore e imprimimos os valores dos nós.