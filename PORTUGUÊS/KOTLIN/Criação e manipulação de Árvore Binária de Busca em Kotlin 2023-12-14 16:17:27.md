Claro! Aqui está um código em Kotlin que cria uma árvore binária de busca e implementa algumas operações básicas, como inserção, busca e remoção de nós.

```kotlin
class No(var valor: Int) {
    var esquerda: No? = null
    var direita: No? = null
}

class ArvoreBinariaBusca() {
    private var raiz: No? = null

    fun inserir(valor: Int) {
        raiz = inserirNo(raiz, valor)
    }

    private fun inserirNo(no: No?, valor: Int): No {
        if (no == null) {
            return No(valor)
        }

        if (valor < no.valor) {
            no.esquerda = inserirNo(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = inserirNo(no.direita, valor)
        }

        return no
    }

    fun buscar(valor: Int): Boolean {
        return buscarNo(raiz, valor)
    }

    private fun buscarNo(no: No?, valor: Int): Boolean {
        if (no == null) {
            return false
        }

        if (valor == no.valor) {
            return true
        }

        return if (valor < no.valor) {
            buscarNo(no.esquerda, valor)
        } else {
            buscarNo(no.direita, valor)
        }
    }

    fun remover(valor: Int) {
        raiz = removerNo(raiz, valor)
    }

    private fun removerNo(no: No?, valor: Int): No? {
        if (no == null) {
            return null
        }

        if (valor < no.valor) {
            no.esquerda = removerNo(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = removerNo(no.direita, valor)
        } else {
            if (no.esquerda == null && no.direita == null) {
                return null
            } else if (no.esquerda == null) {
                return no.direita
            } else if (no.direita == null) {
                return no.esquerda
            } else {
                val menorValor = encontrarMenorValor(no.direita!!)
                no.valor = menorValor
                no.direita = removerNo(no.direita, menorValor)
            }
        }

        return no
    }

    private fun encontrarMenorValor(no: No): Int {
        var atual = no
        while (atual.esquerda != null) {
            atual = atual.esquerda!!
        }
        return atual.valor
    }
}
```

Explicação do código:
- A classe `No` representa um nó na árvore binária e possui um valor inteiro, bem como referências aos nós filhos esquerdo e direito.
- A classe `ArvoreBinariaBusca` é a classe principal que contém a raiz da árvore e implementa as operações de inserção, busca e remoção.
- O método `inserir` é responsável por criar um novo nó com o valor fornecido e inseri-lo na árvore, seguindo as regras da árvore binária de busca.
- O método `buscar` verifica se um determinado valor está presente na árvore, percorrendo a árvore de forma recursiva.
- O método `remover` remove o nó com o valor fornecido da árvore, mantendo a estrutura correta da árvore binária de busca.
- O método `encontrarMenorValor` é uma função auxiliar utilizada no processo de remoção para encontrar o menor valor na subárvore direita de um nó.

Espero que este código atenda às suas expectativas!