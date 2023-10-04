Claro! Aqui está um código complexo em Groovy, que possui uma abordagem diferenciada usando programação funcional e recursos avançados da linguagem:

```groovy
// Função recursiva para calcular o fatorial de um número
def calcFatorial = { num ->
    if (num == 0) {
        return 1
    } else {
        return num * calcFatorial(num - 1)
    }
}

// Função de ordem superior para calcular o somatório dos fatoriais de uma lista de números
def calcSomatorioFatoriais = { lista ->
    lista.collect { calcFatorial(it) }.sum()
}

// Função para verificar se um número é primo
def isPrimo = { num ->
    if (num < 2) {
        return false
    }
    for (int i = 2; i <= Math.sqrt(num); i++) {
        if (num % i == 0) {
            return false
        }
    }
    return true
}

// Função de ordem superior para filtrar os números primos de uma lista
def filtrarPrimos = { lista ->
    lista.findAll { isPrimo(it) }
}

// Função para calcular a média dos números de uma lista
def calcMedia = { lista ->
    lista.sum() / lista.size()
}

// Função para imprimir os resultados
def imprimirResultados = { lista ->
    println("Lista original: $lista")
    def primos = filtrarPrimos(lista)
    println("Números primos: $primos")
    def somatorioFatoriais = calcSomatorioFatoriais(primos)
    println("Somatório dos fatoriais dos números primos: $somatorioFatoriais")
    def media = calcMedia(lista)
    println("Média dos números: $media")
}

// Lista de números para teste
def numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Execução do código
imprimirResultados(numeros)
```

Neste código, utilizamos funções de ordem superior, que são funções que recebem outras funções como parâmetros ou retornam funções. Essas funções de ordem superior nos permitem realizar operações complexas de maneira mais concisa e expressiva.

A função `calcFatorial` é uma função recursiva que calcula o fatorial de um número. Ela utiliza uma estrutura condicional para determinar o caso base (se o número for igual a 0, retorna 1) e, caso contrário, chama a si mesma com um parâmetro decrementado.

A função `calcSomatorioFatoriais` recebe uma lista de números e usa a função `collect` para aplicar a função `calcFatorial` a cada elemento da lista, retornando uma nova lista com os fatoriais. Em seguida, a função `sum` é utilizada para calcular a soma dos fatoriais.

A função `isPrimo` verifica se um número é primo. Ela utiliza uma estrutura condicional para identificar os casos especiais em que o número é menor que 2, retornando `false`. Em seguida, utiliza um loop `for` para verificar se o número é divisível por algum número entre 2 e a raiz quadrada do número. Se encontrar um divisor, retorna `false`, caso contrário, retorna `true`.

A função `filtrarPrimos` recebe uma lista de números e utiliza a função `findAll` para filtrar apenas os números primos da lista, utilizando a função `isPrimo`.

A função `calcMedia` calcula a média dos números de uma lista. Ela utiliza as funções `sum` e `size` para calcular a soma e o tamanho da lista, respectivamente, e realiza a divisão.

Por fim, a função `imprimirResultados` recebe uma lista de números, imprime a lista original, chama a função `filtrarPrimos` para obter os números primos, imprime os números primos, chama a função `calcSomatorioFatoriais` para calcular o somatório dos fatoriais dos números primos e imprime o resultado, e finalmente chama a função `calcMedia` para calcular a média dos números e imprime o resultado.

No exemplo de execução do código, a lista de números `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]` é passada para a função `imprimirResultados`, que realiza todas as operações e imprime os resultados na saída padrão.