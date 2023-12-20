Claro! Aqui está um código complexo em Groovy que realiza uma busca binária em uma lista ordenada:

```groovy
import java.util.Arrays

def buscaBinaria(lista, valor) {
    def inicio = 0
    def fim = lista.size() - 1

    while (inicio <= fim) {
        def meio = (inicio + fim) / 2

        if (lista[meio] == valor) {
            return meio
        }
        if (lista[meio] < valor) {
            inicio = meio + 1
        } else {
            fim = meio - 1
        }
    }
    return -1
}

def lista = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

def valor = 5
def resultado = buscaBinaria(lista, valor)

if (resultado == -1) {
    println("O valor $valor não foi encontrado na lista.")
} else {
    println("O valor $valor foi encontrado na posição $resultado da lista.")
}
```

Neste código, começamos importando a classe `Arrays` do pacote `java.util`, que nos permite usar a função `asList` para criar uma lista com os valores desejados. Em seguida, definimos a função `buscaBinaria`, que recebe a lista e o valor a ser buscado como parâmetros.

Dentro da função, inicializamos duas variáveis `inicio` e `fim` para representar os índices inicial e final da lista. Em um loop `while`, fazemos a comparação do valor do elemento do meio da lista com o valor procurado. Se forem iguais, retornamos o índice do elemento. Caso o valor seja menor, atualizamos o valor de `inicio` para o próximo índice após o meio. Caso contrário, atualizamos o valor de `fim` para o índice anterior ao meio.

Se o valor não for encontrado na lista, a função retorna -1.

No final do código, declaramos uma lista ordenada de números de 1 a 10. Em seguida, definimos o valor a ser buscado como 5 e chamamos a função `buscaBinaria` passando a lista e o valor como argumentos. O resultado é armazenado na variável `resultado`.

Por fim, fazemos uma verificação do valor de `resultado`. Se for -1, exibimos uma mensagem informando que o valor não foi encontrado na lista. Caso contrário, exibimos uma mensagem informando que o valor foi encontrado na posição correspondente na lista.