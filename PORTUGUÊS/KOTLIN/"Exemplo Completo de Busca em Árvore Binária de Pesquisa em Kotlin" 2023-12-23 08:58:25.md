Certamente! Aqui está um exemplo complexo de código em Kotlin que realiza uma operação de busca em uma árvore binária de pesquisa:

```kotlin
class No(var valor: Int, var esquerda: No? = null, var direita: No? = null)

class ArvoreBinaria {
    private var raiz: No? = null
    
    fun inserir(valor: Int) {
        raiz = inserirRecursivo(raiz, valor)
    }
    
    private fun inserirRecursivo(no: No?, valor: Int): No {
        if (no == null) {
            return No(valor)
        }
        
        if (valor < no.valor) {
            no.esquerda = inserirRecursivo(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = inserirRecursivo(no.direita, valor)
        }
        
        return no
    }
    
    fun buscar(valor: Int): Boolean {
        return buscarRecursivo(raiz, valor)
    }
    
    private fun buscarRecursivo(no: No?, valor: Int): Boolean {
        if (no == null) {
            return false
        }
        
        if (valor == no.valor) {
            return true
        }
        
        return if (valor < no.valor) {
            buscarRecursivo(no.esquerda, valor)
        } else {
            buscarRecursivo(no.direita, valor)
        }
    }
}

fun main() {
    val arvore = ArvoreBinaria()
    
    arvore.inserir(10)
    arvore.inserir(5)
    arvore.inserir(15)
    arvore.inserir(3)
    arvore.inserir(7)
    arvore.inserir(12)
    arvore.inserir(20)
    
    val valorBuscado = 7
    if (arvore.buscar(valorBuscado)) {
        println("O valor $valorBuscado foi encontrado na árvore binária.")
    } else {
        println("O valor $valorBuscado não foi encontrado na árvore binária.")
    }
}
```

Neste código, é criada uma classe `No` que representa um nó em uma árvore binária de pesquisa. Cada nó possui um valor inteiro, uma referência ao nó da esquerda e uma referência ao nó da direita. 

Em seguida, é criada a classe `ArvoreBinaria` que contém um campo para a raiz da árvore. Ela possui dois métodos principais: `inserir` e `buscar`.

O método `inserir` é responsável por inserir um novo valor na árvore binária. Ele utiliza o método `inserirRecursivo` para realizar a inserção de forma recursiva, percorrendo a árvore até encontrar a posição correta para o novo valor.

O método `buscar` recebe um valor como parâmetro e retorna `true` se o valor estiver presente na árvore ou `false` caso contrário. Ele utiliza o método `buscarRecursivo` para realizar a busca de forma recursiva, percorrendo a árvore até encontrar o valor desejado ou chegar a um nó nulo.

No `main`, é criada uma instância da classe `ArvoreBinaria` e alguns valores são inseridos na árvore. Em seguida, é definido um valor a ser buscado na árvore e é feita a chamada ao método `buscar`. O resultado da busca é então impresso no console.

Espero que este exemplo atenda às suas expectativas de complexidade e diferenciação. Caso tenha alguma dúvida ou precise de mais informações, fique à vontade para perguntar!