Certamente! Aqui está um exemplo de código em Swift que permite criar e manipular uma árvore binária de busca:

```swift
// Definição da estrutura do nó da árvore
class Node<T: Comparable> {
    var value: T
    var left: Node?
    var right: Node?
    
    init(value: T) {
        self.value = value
    }
}

// Definição da estrutura da árvore binária de busca
class BinarySearchTree<T: Comparable> {
    var root: Node<T>?
    
    // Função para inserir um valor na árvore
    func insert(value: T) {
        let newNode = Node(value: value)
        
        if let root = root {
            insertNode(node: root, newNode: newNode)
        } else {
            root = newNode
        }
    }
    
    // Função auxiliar para inserir um nó na árvore
    private func insertNode(node: Node<T>, newNode: Node<T>) {
        if newNode.value < node.value {
            if let left = node.left {
                insertNode(node: left, newNode: newNode)
            } else {
                node.left = newNode
            }
        } else if newNode.value > node.value {
            if let right = node.right {
                insertNode(node: right, newNode: newNode)
            } else {
                node.right = newNode
            }
        }
    }
    
    // Função para buscar um valor na árvore
    func search(value: T) -> Bool {
        guard let root = root else {
            return false
        }
        
        return searchNode(node: root, value: value)
    }
    
    // Função auxiliar para buscar um valor na árvore
    private func searchNode(node: Node<T>, value: T) -> Bool {
        if value == node.value {
            return true
        } else if value < node.value {
            if let left = node.left {
                return searchNode(node: left, value: value)
            } else {
                return false
            }
        } else {
            if let right = node.right {
                return searchNode(node: right, value: value)
            } else {
                return false
            }
        }
    }
    
    // Função para percorrer a árvore em ordem (in-order traversal)
    func inorderTraversal() {
        guard let root = root else {
            return
        }
        
        inorderTraversal(node: root)
    }
    
    // Função auxiliar para percorrer a árvore em ordem
    private func inorderTraversal(node: Node<T>) {
        if let left = node.left {
            inorderTraversal(node: left)
        }
        
        print(node.value)
        
        if let right = node.right {
            inorderTraversal(node: right)
        }
    }
}

// Exemplo de uso da árvore binária de busca
let bst = BinarySearchTree<Int>()
bst.insert(value: 5)
bst.insert(value: 3)
bst.insert(value: 7)
bst.insert(value: 2)
bst.insert(value: 4)
bst.insert(value: 6)
bst.insert(value: 8)

print(bst.search(value: 6)) // Output: true
print(bst.search(value: 9)) // Output: false

bst.inorderTraversal() // Output: 2 3 4 5 6 7 8
```

Neste código, estamos criando uma estrutura de dados de árvore binária de busca em Swift. A árvore é composta por nós, onde cada nó possui um valor, um filho esquerdo e um filho direito. A árvore é organizada de forma que todos os valores menores que um nó são armazenados no filho esquerdo, enquanto os valores maiores são armazenados no filho direito.

O código inclui funções para inserir valores na árvore, buscar um valor específico, e percorrer a árvore em ordem (in-order traversal). No exemplo de uso, estamos inserindo alguns valores na árvore e realizando uma busca por um valor específico. Também estamos percorrendo a árvore em ordem e imprimindo os valores.