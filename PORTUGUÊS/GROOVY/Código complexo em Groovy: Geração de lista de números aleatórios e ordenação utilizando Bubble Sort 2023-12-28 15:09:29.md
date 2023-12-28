Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

def listaNumeros = []

// Gerando uma lista de números aleatórios
def gerarNumeros() {
    def random = new Random()
    
    100000.times {
        listaNumeros << random.nextInt(1000)
    }
}

// Ordenando a lista de números utilizando o algoritmo Bubble Sort
def ordenarNumeros() {
    def tamanho = listaNumeros.size()
    
    for (int i = 0; i < tamanho - 1; i++) {
        for (int j = 0; j < tamanho - i - 1; j++) {
            if (listaNumeros[j] > listaNumeros[j + 1]) {
                def temp = listaNumeros[j]
                listaNumeros[j] = listaNumeros[j + 1]
                listaNumeros[j + 1] = temp
            }
        }
    }
}

// Verificando se um número existe na lista
def verificarNumero(numero) {
    def encontrado = false
    
    for (int i = 0; i < listaNumeros.size(); i++) {
        if (listaNumeros[i] == numero) {
            encontrado = true
            break
        }
    }
    
    if (encontrado) {
        println("O número ${numero} foi encontrado na lista!")
    } else {
        println("O número ${numero} não foi encontrado na lista!")
    }
}

// Imprimindo a lista ordenada
def imprimirLista() {
    listaNumeros.each {
        print("${it} ")
    }
    println()
}

// Executando as funções
gerarNumeros()
ordenarNumeros()
verificarNumero(500)
imprimirLista()
```

Neste código em Groovy, estamos gerando uma lista de 100.000 números aleatórios utilizando a classe `Random`. Em seguida, utilizamos o algoritmo Bubble Sort para ordenar a lista em ordem crescente.

Após a ordenação, utilizamos a função `verificarNumero(numero)` para procurar um número específico na lista. Se o número for encontrado, uma mensagem é exibida indicando que o número foi encontrado. Caso contrário, uma mensagem é exibida informando que o número não foi encontrado.

Por fim, utilizamos a função `imprimirLista()` para imprimir a lista ordenada no console.