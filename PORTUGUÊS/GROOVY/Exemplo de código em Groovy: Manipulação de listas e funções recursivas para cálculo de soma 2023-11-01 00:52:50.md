Claro! Aqui está um exemplo de código em Groovy que envolve manipulação de listas e uso de funções recursivas para calcular a soma de todos os elementos de uma lista:

```groovy
// Função recursiva para calcular a soma de todos os elementos de uma lista
def somaLista(lista) {
    if (lista.isEmpty()) {
        return 0
    } else {
        return lista.head() + somaLista(lista.tail())
    }
}

// Função para gerar uma lista com números pares de 0 até um valor máximo
def gerarListaPares(max) {
    def lista = []
    for (int i = 0; i <= max; i += 2) {
        lista.add(i)
    }
    return lista
}

// Função para gerar uma lista com números ímpares de 1 até um valor máximo
def gerarListaImpares(max) {
    def lista = []
    for (int i = 1; i <= max; i += 2) {
        lista.add(i)
    }
    return lista
}

// Função para gerar uma lista com números primos de 2 até um valor máximo
def gerarListaPrimos(max) {
    def lista = []
    for (int i = 2; i <= max; i++) {
        def isPrimo = true
        for (int j = 2; j < i; j++) {
            if (i % j == 0) {
                isPrimo = false
                break
            }
        }
        if (isPrimo) {
            lista.add(i)
        }
    }
    return lista
}

// Gerar uma lista de números pares de 0 até 10
def listaPares = gerarListaPares(10)
println("Lista de números pares: $listaPares")

// Calcular a soma de todos os elementos da lista de números pares
def somaPares = somaLista(listaPares)
println("Soma dos números pares: $somaPares")

// Gerar uma lista de números ímpares de 1 até 10
def listaImpares = gerarListaImpares(10)
println("Lista de números ímpares: $listaImpares")

// Calcular a soma de todos os elementos da lista de números ímpares
def somaImpares = somaLista(listaImpares)
println("Soma dos números ímpares: $somaImpares")

// Gerar uma lista de números primos de 2 até 20
def listaPrimos = gerarListaPrimos(20)
println("Lista de números primos: $listaPrimos")

// Calcular a soma de todos os elementos da lista de números primos
def somaPrimos = somaLista(listaPrimos)
println("Soma dos números primos: $somaPrimos")
```

Neste código em Groovy, temos quatro funções principais: `somaLista`, `gerarListaPares`, `gerarListaImpares` e `gerarListaPrimos`. 

A função `somaLista` é uma função recursiva que recebe uma lista como parâmetro e retorna a soma de todos os seus elementos. Ela utiliza a recursão para somar o primeiro elemento da lista com a soma do restante dos elementos.

As funções `gerarListaPares`, `gerarListaImpares` e `gerarListaPrimos` são responsáveis por gerar listas com números pares, ímpares e primos, respectivamente. Elas utilizam loops `for` para iterar sobre os números e adicioná-los à lista, de acordo com a condição específica para cada tipo de número.

Após a definição das funções, o código chama cada uma delas para gerar diferentes listas e calcular a soma de seus elementos. Em seguida, os resultados são impressos no console.

Esse código permite explorar o uso de funções recursivas e manipulação de listas em Groovy, além de gerar diferentes tipos de listas e calcular suas somas.