Com certeza! Aqui está um código em Groovy que cria uma árvore binária de números aleatórios e, em seguida, percorre a árvore para imprimir os números em ordem decrescente:

```groovy
import java.util.Random

class No {
    int valor
    No esquerda
    No direita
    
    No(int valor) {
        this.valor = valor
        esquerda = null
        direita = null
    }
}

class ArvoreBinaria {
    No raiz
    
    void adicionar(int valor) {
        raiz = adicionarRecursivamente(raiz, valor)
    }
    
    private No adicionarRecursivamente(No atual, int valor) {
        if (atual == null) {
            return new No(valor)
        }
        
        if (valor < atual.valor) {
            atual.esquerda = adicionarRecursivamente(atual.esquerda, valor)
        } else if (valor > atual.valor) {
            atual.direita = adicionarRecursivamente(atual.direita, valor)
        }
        
        return atual
    }
    
    void imprimirDecrescente() {
        imprimirDecrescenteRecursivamente(raiz)
    }
    
    private void imprimirDecrescenteRecursivamente(No atual) {
        if (atual != null) {
            imprimirDecrescenteRecursivamente(atual.direita)
            println(atual.valor)
            imprimirDecrescenteRecursivamente(atual.esquerda)
        }
    }
}

def arvore = new ArvoreBinaria()

def random = new Random()
def numeros = []

// Gerando números aleatórios
for (int i = 0; i < 10; i++) {
    numeros << random.nextInt(100)
}

// Adicionando os números na árvore
numeros.each {
    arvore.adicionar(it)
}

// Imprimindo os números em ordem decrescente
arvore.imprimirDecrescente()
```

Neste código, começo definindo uma classe `No` para representar cada nó da árvore binária. Cada nó possui um valor inteiro e referências para os nós filhos esquerda e direita.

Em seguida, defino uma classe `ArvoreBinaria` que contém a raiz da árvore. A classe possui um método `adicionar` que adiciona um novo valor à árvore, mantendo a ordem, e um método `imprimirDecrescente` que percorre a árvore de forma recursiva, imprimindo os valores em ordem decrescente.

No programa principal, crio uma instância da classe `ArvoreBinaria` e gero 10 números aleatórios usando a classe `Random`. Em seguida, adiciono os números na árvore e, por fim, imprimo os números em ordem decrescente.

Espero que este código atenda às suas expectativas!