Claro! Aqui está um exemplo de código em Kotlin que implementa uma árvore binária de pesquisa (BST) com algumas funcionalidades adicionais:

```kotlin
class No(var valor: Int) {
    var esquerda: No? = null
    var direita: No? = null
}

class BST {
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

    fun remover(valor: Int) {
        raiz = removerRecursivo(raiz, valor)
    }

    private fun removerRecursivo(no: No?, valor: Int): No? {
        if (no == null) {
            return no
        }

        if (valor < no.valor) {
            no.esquerda = removerRecursivo(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = removerRecursivo(no.direita, valor)
        } else {
            if (no.esquerda == null) {
                return no.direita
            } else if (no.direita == null) {
                return no.esquerda
            }

            no.valor = encontrarMenorValor(no.direita!!)
            no.direita = removerRecursivo(no.direita, no.valor)
        }

        return no
    }

    private fun encontrarMenorValor(no: No): Int {
        return if (no.esquerda == null) {
            no.valor
        } else {
            encontrarMenorValor(no.esquerda!!)
        }
    }

    fun imprimirEmOrdem() {
        imprimirEmOrdemRecursivo(raiz)
    }

    private fun imprimirEmOrdemRecursivo(no: No?) {
        if (no != null) {
            imprimirEmOrdemRecursivo(no.esquerda)
            println(no.valor)
            imprimirEmOrdemRecursivo(no.direita)
        }
    }
}
```

Este código implementa uma árvore binária de pesquisa (BST) em Kotlin. A classe `No` representa um nó na árvore, contendo um valor inteiro, bem como referências para seus filhos esquerdo e direito. A classe `BST` é a implementação da árvore binária de pesquisa, contendo métodos para inserir, buscar, remover e imprimir os elementos.

O método `inserir` permite adicionar um novo valor à árvore. Ele utiliza um método auxiliar `inserirRecursivo` para percorrer a árvore de forma recursiva e encontrar a posição correta para inserir o novo valor.

O método `buscar` verifica se um determinado valor está presente na árvore. Ele também utiliza um método auxiliar `buscarRecursivo` para percorrer a árvore de forma recursiva.

O método `remover` permite remover um valor da árvore. Ele utiliza um método auxiliar `removerRecursivo` que percorre a árvore de forma recursiva e remove o nó contendo o valor especificado. Se necessário, ele encontra o menor valor na subárvore direita do nó removido para substituí-lo.

O método `imprimirEmOrdem` imprime os elementos da árvore em ordem crescente. Ele utiliza um método auxiliar `imprimirEmOrdemRecursivo` para percorrer a árvore de forma recursiva e imprimir os valores.

Essa é apenas uma implementação básica de uma árvore binária de pesquisa em Kotlin. Existem várias outras funcionalidades e otimizações que podem ser adicionadas, dependendo dos requisitos específicos do projeto.