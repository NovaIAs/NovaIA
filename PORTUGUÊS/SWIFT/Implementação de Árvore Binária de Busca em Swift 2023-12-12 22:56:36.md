Claro! Aqui está um exemplo de um código em Swift que implementa uma árvore binária de busca e realiza a busca de um elemento específico:

```swift
// Definição da estrutura do nó da árvore binária de busca
class Node {
    var value: Int
    var left: Node?
    var right: Node?
    
    init(_ value: Int) {
        self.value = value
        self.left = nil
        self.right = nil
    }
}

// Definição da árvore binária de busca
class BinarySearchTree {
    var root: Node?
    
    // Função para inserir um elemento na árvore
    func insert(_ value: Int) {
        let newNode = Node(value)
        
        if root == nil {
            root = newNode
        } else {
            insertNode(root, newNode)
        }
    }
    
    // Função auxiliar para inserir um nó na árvore
    private func insertNode(_ node: Node?, _ newNode: Node) {
        if let node = node {
            if newNode.value < node.value {
                if node.left == nil {
                    node.left = newNode
                } else {
                    insertNode(node.left, newNode)
                }
            } else {
                if node.right == nil {
                    node.right = newNode
                } else {
                    insertNode(node.right, newNode)
                }
            }
        }
    }
    
    // Função para realizar a busca de um elemento na árvore
    func search(_ value: Int) -> Bool {
        return searchNode(root, value)
    }
    
    // Função auxiliar para realizar a busca de um nó na árvore
    private func searchNode(_ node: Node?, _ value: Int) -> Bool {
        if let node = node {
            if value == node.value {
                return true
            } else if value < node.value {
                return searchNode(node.left, value)
            } else {
                return searchNode(node.right, value)
            }
        }
        
        return false
    }
}

// Exemplo de uso da árvore binária de busca
let tree = BinarySearchTree()
tree.insert(5)
tree.insert(2)
tree.insert(7)
tree.insert(1)
tree.insert(3)

if tree.search(3) {
    print("O elemento 3 está presente na árvore!")
} else {
    print("O elemento 3 não está presente na árvore!")
}

if tree.search(6) {
    print("O elemento 6 está presente na árvore!")
} else {
    print("O elemento 6 não está presente na árvore!")
}
```

Neste exemplo, é definida a estrutura de um nó da árvore binária de busca através da classe `Node`, contendo o valor do nó, a referência para o nó filho esquerdo e a referência para o nó filho direito.

A classe `BinarySearchTree` representa a árvore binária de busca, contendo o nó raiz e as funções `insert` para inserir um elemento na árvore e `search` para realizar a busca de um elemento específico.

A função `insert` verifica se a árvore está vazia e, se estiver, atribui o novo nó como raiz. Caso contrário, chama a função auxiliar `insertNode` para realizar a inserção de forma recursiva.

A função `insertNode` verifica se o valor do novo nó é menor ou maior que o valor do nó atual e, com base nisso, avança para o nó filho esquerdo ou direito até encontrar a posição correta para inserir o novo nó.

A função `search` chama a função auxiliar `searchNode` para realizar a busca de forma recursiva, verificando se o valor do nó atual é igual, menor ou maior que o valor buscado. Se for igual, retorna `true`. Caso contrário, avança para o nó filho esquerdo ou direito até encontrar o valor buscado ou percorrer toda a árvore.

No exemplo de uso da árvore binária de busca, são inseridos alguns elementos e posteriormente é realizada a busca pelos valores 3 e 6, exibindo uma mensagem indicando se os elementos estão presentes na árvore ou não.

Espero que este código seja útil para você! Lembre-se de adaptá-lo conforme suas necessidades.