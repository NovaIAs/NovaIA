Claro! Aqui está um código complexo em Groovy que gera uma árvore binária de forma recursiva:

```groovy
class Node {
    def value
    def leftChild
    def rightChild
    
    Node(def value) {
        this.value = value
        leftChild = null
        rightChild = null
    }
}

class BinaryTree {
    def root
    
    BinaryTree() {
        root = null
    }
    
    def insert(def value) {
        root = insertRecursive(root, value)
    }
    
    def insertRecursive(def currentNode, def value) {
        if (currentNode == null) {
            return new Node(value)
        }
        
        if (value < currentNode.value) {
            currentNode.leftChild = insertRecursive(currentNode.leftChild, value)
        } else if (value > currentNode.value) {
            currentNode.rightChild = insertRecursive(currentNode.rightChild, value)
        }
        
        return currentNode
    }
    
    def search(def value) {
        return searchRecursive(root, value)
    }
    
    def searchRecursive(def currentNode, def value) {
        if (currentNode == null || currentNode.value == value) {
            return currentNode
        }
        
        if (value < currentNode.value) {
            return searchRecursive(currentNode.leftChild, value)
        }
        
        return searchRecursive(currentNode.rightChild, value)
    }
    
    def delete(def value) {
        root = deleteRecursive(root, value)
    }
    
    def deleteRecursive(def currentNode, def value) {
        if (currentNode == null) {
            return currentNode
        }
        
        if (value < currentNode.value) {
            currentNode.leftChild = deleteRecursive(currentNode.leftChild, value)
        } else if (value > currentNode.value) {
            currentNode.rightChild = deleteRecursive(currentNode.rightChild, value)
        } else {
            if (currentNode.leftChild == null) {
                return currentNode.rightChild
            } else if (currentNode.rightChild == null) {
                return currentNode.leftChild
            }
            
            currentNode.value = findMinValue(currentNode.rightChild)
            currentNode.rightChild = deleteRecursive(currentNode.rightChild, currentNode.value)
        }
        
        return currentNode
    }
    
    def findMinValue(def currentNode) {
        def minValue = currentNode.value
        
        while (currentNode.leftChild != null) {
            minValue = currentNode.leftChild.value
            currentNode = currentNode.leftChild
        }
        
        return minValue
    }
}

def binaryTree = new BinaryTree()
binaryTree.insert(50)
binaryTree.insert(30)
binaryTree.insert(20)
binaryTree.insert(40)
binaryTree.insert(70)
binaryTree.insert(60)
binaryTree.insert(80)

println "Árvore binária criada!"
println "Inserindo valores na árvore: 50, 30, 20, 40, 70, 60, 80"

println "Buscando valor 40 na árvore: " + binaryTree.search(40)
println "Buscando valor 90 na árvore: " + binaryTree.search(90)

println "Removendo valor 30 da árvore..."
binaryTree.delete(30)
println "Buscando valor 30 na árvore: " + binaryTree.search(30)
```

Este código cria uma classe `Node` que representa um nó na árvore binária, contendo um atributo `value` para armazenar o valor do nó, e dois atributos `leftChild` e `rightChild` que representam os filhos esquerdo e direito do nó, respectivamente. 

A classe `BinaryTree` representa a árvore binária em si, com um atributo `root` que armazena o nó raiz da árvore. Ela possui três métodos principais: `insert`, `search` e `delete`. 

O método `insert` insere um novo valor na árvore de forma recursiva. Ele verifica se o nó atual é nulo, caso seja, cria um novo nó com o valor passado como parâmetro. Caso contrário, compara o valor com o valor do nó atual e decide se deve ser inserido à esquerda ou à direita do nó atual, chamando o método `insertRecursive` para o nó filho correspondente.

O método `search` realiza uma busca na árvore de forma recursiva. Ele verifica se o nó atual é nulo ou se o valor do nó atual é igual ao valor buscado. Caso seja, retorna o nó atual. Caso contrário, compara o valor com o valor do nó atual e chama o método `searchRecursive` para o nó filho correspondente.

O método `delete` remove um valor da árvore de forma recursiva. Ele verifica se o nó atual é nulo. Caso seja, retorna o nó atual. Caso contrário, compara o valor com o valor do nó atual e decide se deve ser removido à esquerda ou à direita do nó atual, chamando o método `deleteRecursive` para o nó filho correspondente. Se o nó a ser removido não possui filhos, ele é removido diretamente. Caso contrário, é substituído pelo menor valor do subárvore direita e o método `deleteRecursive` é chamado novamente para remover o nó com o valor substituído.

Por fim, o código cria uma instância da classe `BinaryTree`, insere alguns valores na árvore, realiza buscas e remove um valor da árvore. Os resultados são impressos no console.