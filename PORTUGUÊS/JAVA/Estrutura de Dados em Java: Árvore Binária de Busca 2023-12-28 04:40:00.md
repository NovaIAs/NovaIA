Claro! Aqui está um exemplo de código em Java que cria uma estrutura de dados chamada "Árvore Binária de Busca" e implementa algumas operações como inserção, busca e remoção de elementos. A árvore binária de busca é uma estrutura de dados que permite armazenar elementos de forma ordenada, facilitando a busca e outras operações.

```java
import java.util.*;

class No {
    int valor;
    No esquerda, direita;

    public No(int item) {
        valor = item;
        esquerda = direita = null;
    }
}

class ArvoreBinariaBusca {
    No raiz;

    ArvoreBinariaBusca() {
        raiz = null;
    }

    void inserir(int valor) {
        raiz = inserirRecursivo(raiz, valor);
    }

    No inserirRecursivo(No no, int valor) {
        if (no == null) {
            no = new No(valor);
            return no;
        }

        if (valor < no.valor)
            no.esquerda = inserirRecursivo(no.esquerda, valor);
        else if (valor > no.valor)
            no.direita = inserirRecursivo(no.direita, valor);

        return no;
    }

    boolean buscar(int valor) {
        return buscarRecursivo(raiz, valor);
    }

    boolean buscarRecursivo(No no, int valor) {
        if (no == null)
            return false;

        if (valor == no.valor)
            return true;

        if (valor < no.valor)
            return buscarRecursivo(no.esquerda, valor);

        return buscarRecursivo(no.direita, valor);
    }

    void remover(int valor) {
        raiz = removerRecursivo(raiz, valor);
    }

    No removerRecursivo(No no, int valor) {
        if (no == null)
            return no;

        if (valor < no.valor)
            no.esquerda = removerRecursivo(no.esquerda, valor);
        else if (valor > no.valor)
            no.direita = removerRecursivo(no.direita, valor);
        else {
            if (no.esquerda == null)
                return no.direita;
            else if (no.direita == null)
                return no.esquerda;

            no.valor = valorMinimo(no.direita);

            no.direita = removerRecursivo(no.direita, no.valor);
        }

        return no;
    }

    int valorMinimo(No no) {
        int valorMin = no.valor;
        while (no.esquerda != null) {
            valorMin = no.esquerda.valor;
            no = no.esquerda;
        }
        return valorMin;
    }

    void visitarEmOrdem(No no) {
        if (no != null) {
            visitarEmOrdem(no.esquerda);
            System.out.print(no.valor + " ");
            visitarEmOrdem(no.direita);
        }
    }

    void imprimirEmOrdem() {
        visitarEmOrdem(raiz);
        System.out.println();
    }

    public static void main(String[] args) {
        ArvoreBinariaBusca arvore = new ArvoreBinariaBusca();

        arvore.inserir(50);
        arvore.inserir(30);
        arvore.inserir(20);
        arvore.inserir(40);
        arvore.inserir(70);
        arvore.inserir(60);
        arvore.inserir(80);

        System.out.println("Árvore Binária de Busca:");
        arvore.imprimirEmOrdem();

        System.out.println("Buscando o valor 40: " + arvore.buscar(40));
        System.out.println("Buscando o valor 90: " + arvore.buscar(90));

        arvore.remover(20);
        arvore.remover(60);
        arvore.remover(80);

        System.out.println("Árvore Binária de Busca após remoção:");
        arvore.imprimirEmOrdem();
    }
}
```

Nesse código, temos duas classes: "No" e "ArvoreBinariaBusca". A classe "No" representa os nós da árvore e possui um valor inteiro, além de referências para os nós filhos esquerdo e direito. A classe "ArvoreBinariaBusca" representa a árvore em si e possui uma referência para a raiz.

A implementação das operações é feita através de métodos como "inserir", "buscar" e "remover". A inserção é feita de forma recursiva, comparando o valor a ser inserido com o valor do nó atual e decidindo se deve ser colocado à esquerda ou à direita. A busca também é feita de forma recursiva, percorrendo a árvore comparando o valor buscado com o valor do nó atual.

A remoção é um pouco mais complexa. Primeiro, encontra-se o nó a ser removido percorrendo a árvore até encontrá-lo. Depois, verifica-se se o nó possui filhos ou não. Caso não possua, simplesmente o remove. Caso possua, substitui-se o valor do nó pela menor chave do subárvore direita (sucessor) e em seguida remove-se esse sucessor.

Por fim, temos um exemplo de uso da árvore binária de busca. São inseridos alguns elementos na árvore e depois são realizadas buscas e remoções.