Claro! Aqui está um código complexo em Kotlin que implementa uma estrutura de dados chamada Árvore AVL.

```
class Node(var chave: Int, var esquerda: Node? = null, var direita: Node? = null, var altura: Int = 1)

class AVLTree {
    private var raiz: Node? = null

    private fun altura(no: Node?): Int {
        return no?.altura ?: 0
    }

    private fun fatorBalanceamento(no: Node?): Int {
        return altura(no?.esquerda) - altura(no?.direita)
    }

    private fun atualizarAltura(no: Node?) {
        no?.altura = maxOf(altura(no?.esquerda), altura(no?.direita)) + 1
    }

    private fun rotacaoEsquerda(z: Node?): Node? {
        val y = z?.direita
        val T2 = y?.esquerda

        y?.esquerda = z
        z?.direita = T2

        atualizarAltura(z)
        atualizarAltura(y)

        return y
    }

    private fun rotacaoDireita(z: Node?): Node? {
        val y = z?.esquerda
        val T3 = y?.direita

        y?.direita = z
        z?.esquerda = T3

        atualizarAltura(z)
        atualizarAltura(y)

        return y
    }

    fun inserir(chave: Int) {
        raiz = inserirNo(raiz, chave)
    }

    private fun inserirNo(no: Node?, chave: Int): Node? {
        if (no == null) {
            return Node(chave)
        }

        if (chave < no.chave) {
            no.esquerda = inserirNo(no.esquerda, chave)
        } else if (chave > no.chave) {
            no.direita = inserirNo(no.direita, chave)
        } else {
            return no
        }

        atualizarAltura(no)

        val fator = fatorBalanceamento(no)

        // Rotação à esquerda
        if (fator > 1 && chave < no.esquerda?.chave ?: 0) {
            return rotacaoDireita(no)
        }

        // Rotação à direita
        if (fator < -1 && chave > no.direita?.chave ?: 0) {
            return rotacaoEsquerda(no)
        }

        // Rotação dupla à esquerda
        if (fator > 1 && chave > no.esquerda?.chave ?: 0) {
            no.esquerda = rotacaoEsquerda(no.esquerda)
            return rotacaoDireita(no)
        }

        // Rotação dupla à direita
        if (fator < -1 && chave < no.direita?.chave ?: 0) {
            no.direita = rotacaoDireita(no.direita)
            return rotacaoEsquerda(no)
        }

        return no
    }
}
```

Este código implementa a estrutura de dados Árvore AVL, uma árvore binária de busca balanceada. A classe `Node` representa um nó da árvore, contendo uma chave, referências para os nós filhos esquerdo e direito, e uma altura que é usada para realizar a balanceamento da árvore.

A classe `AVLTree` contém os métodos necessários para realizar as operações de inserção na árvore. O método `inserir` recebe uma chave e chama o método privado `inserirNo` para realizar a inserção recursivamente.

Ao inserir um novo nó na árvore, o código realiza as rotações necessárias para manter a propriedade de balanceamento da árvore AVL. As rotações são: rotação à esquerda, rotação à direita, rotação dupla à esquerda e rotação dupla à direita.

A altura de cada nó é atualizada após cada inserção e rotação, garantindo que o fator de balanceamento seja corretamente calculado. O fator de balanceamento é usado para determinar qual rotação deve ser realizada para manter a árvore balanceada.

Este código é complexo porque implementa uma estrutura de dados avançada e usa conceitos como recursão, rotações e fatores de balanceamento. É uma implementação completa e otimizada da Árvore AVL em Kotlin.