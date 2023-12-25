Certamente! Abaixo está um código complexo em Groovy que tem como objetivo criar uma árvore binária de pesquisa e implementar algumas operações nessa estrutura de dados.

```groovy
class No {
    def valor
    def esquerda
    def direita

    No(valor) {
        this.valor = valor
        esquerda = null
        direita = null
    }
}

class ArvoreBinaria {
    def raiz

    ArvoreBinaria() {
        raiz = null
    }

    def adicionar(valor) {
        raiz = adicionarRecursivamente(raiz, valor)
    }

    private def adicionarRecursivamente(no, valor) {
        if (no == null) {
            return new No(valor)
        }

        if (valor < no.valor) {
            no.esquerda = adicionarRecursivamente(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = adicionarRecursivamente(no.direita, valor)
        }

        return no
    }

    def buscar(valor) {
        return buscarRecursivamente(raiz, valor)
    }

    private def buscarRecursivamente(no, valor) {
        if (no == null || no.valor == valor) {
            return no
        }

        if (valor < no.valor) {
            return buscarRecursivamente(no.esquerda, valor)
        }

        return buscarRecursivamente(no.direita, valor)
    }

    def remover(valor) {
        raiz = removerRecursivamente(raiz, valor)
    }

    private def removerRecursivamente(no, valor) {
        if (no == null) {
            return no
        }

        if (valor < no.valor) {
            no.esquerda = removerRecursivamente(no.esquerda, valor)
        } else if (valor > no.valor) {
            no.direita = removerRecursivamente(no.direita, valor)
        } else {
            if (no.esquerda == null) {
                return no.direita
            } else if (no.direita == null) {
                return no.esquerda
            }

            no.valor = encontrarMenorValor(no.direita)
            no.direita = removerRecursivamente(no.direita, no.valor)
        }

        return no
    }

    private def encontrarMenorValor(no) {
        while (no.esquerda != null) {
            no = no.esquerda
        }

        return no.valor
    }

    def imprimirEmOrdem() {
        imprimirEmOrdemRecursivamente(raiz)
    }

    private def imprimirEmOrdemRecursivamente(no) {
        if (no != null) {
            imprimirEmOrdemRecursivamente(no.esquerda)
            print(no.valor + " ")
            imprimirEmOrdemRecursivamente(no.direita)
        }
    }
}

def arvore = new ArvoreBinaria()
arvore.adicionar(50)
arvore.adicionar(30)
arvore.adicionar(70)
arvore.adicionar(20)
arvore.adicionar(40)
arvore.adicionar(60)
arvore.adicionar(80)

println("Árvore em ordem:")
arvore.imprimirEmOrdem()

println("Buscando o valor 40:")
def noEncontrado = arvore.buscar(40)
if (noEncontrado != null) {
    println("Valor encontrado: " + noEncontrado.valor)
} else {
    println("Valor não encontrado")
}

println("Removendo o valor 30:")
arvore.remover(30)
println("Árvore em ordem após a remoção:")
arvore.imprimirEmOrdem()
```

Neste código, criamos duas classes: `No`, que representa um nó da árvore, e `ArvoreBinaria`, que contém a implementação das operações da árvore binária de pesquisa.

A classe `No` possui três atributos: `valor`, que guarda o valor do nó, `esquerda`, que aponta para o nó à esquerda, e `direita`, que aponta para o nó à direita. O construtor da classe `No` inicializa esses atributos.

A classe `ArvoreBinaria` possui um atributo `raiz`, que aponta para o nó raiz da árvore. O construtor da classe inicia a raiz como nulo.

O método `adicionar` recebe um valor e insere um novo nó na árvore binária de forma recursiva. O método `adicionarRecursivamente` é chamado internamente para percorrer a árvore e encontrar a posição correta para inserir o novo nó.

O método `buscar` recebe um valor e retorna o nó correspondente, caso ele exista na árvore. O método `buscarRecursivamente` é chamado internamente para percorrer a árvore de forma recursiva e encontrar o nó desejado.

O método `remover` recebe um valor e remove o nó correspondente da árvore, caso ele exista. O método `removerRecursivamente` é chamado internamente para percorrer a árvore de forma recursiva e realizar a remoção.

O método `imprimirEmOrdem` realiza uma travessia em ordem na árvore e imprime os valores dos nós em ordem crescente. O método `imprimirEmOrdemRecursivamente` é chamado internamente para percorrer a árvore de forma recursiva e imprimir os valores.

No final do código, criamos uma instância da classe `ArvoreBinaria`, adicionamos alguns valores, imprimimos a árvore em ordem, buscamos um valor específico e removemos um valor. Por fim, imprimimos novamente a árvore em ordem para verificar as alterações.

Espero que este exemplo atenda às suas expectativas!