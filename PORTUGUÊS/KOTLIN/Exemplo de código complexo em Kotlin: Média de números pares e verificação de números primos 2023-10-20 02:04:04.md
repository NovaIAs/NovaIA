Claro! Aqui está um exemplo de código complexo em Kotlin:

```
fun main() {
    val listaNumeros = mutableListOf<Int>()
    
    // Preenche a lista com números de 1 a 100
    for (i in 1..100) {
        listaNumeros.add(i)
    }
    
    // Filtra apenas os números pares
    val listaPares = listaNumeros.filter { it % 2 == 0 }
    
    // Calcula a média dos números pares
    val mediaPares = listaPares.average()
    
    // Imprime a média dos números pares
    println("A média dos números pares é: $mediaPares")
    
    // Verifica se algum número é primo
    val possuiPrimo = listaNumeros.any { numero ->
        var divisores = 0
        for (i in 1..numero) {
            if (numero % i == 0) {
                divisores++
            }
        }
        divisores == 2
    }
    
    // Imprime se a lista possui algum número primo
    if (possuiPrimo) {
        println("A lista possui números primos")
    } else {
        println("A lista não possui números primos")
    }
}
```

Explicação do código:
1. Criamos uma função `main` que será o ponto de entrada do programa.
2. Inicializamos uma lista vazia chamada `listaNumeros` usando o tipo `MutableList<Int>`, que permite adicionar, remover e modificar elementos.
3. Utilizamos um loop `for` para preencher a lista com números de 1 a 100.
4. Utilizamos a função `filter` para filtrar apenas os números pares da lista, utilizando a expressão lambda `{ it % 2 == 0 }`.
5. Calculamos a média dos números pares utilizando a função `average`.
6. Imprimimos a média dos números pares utilizando a função `println` e interpolando a variável `$mediaPares`.
7. Utilizamos a função `any` para verificar se algum número da lista é primo. Para isso, utilizamos uma expressão lambda que verifica se o número possui exatamente dois divisores.
8. Imprimimos se a lista possui ou não números primos utilizando a função `println` e uma condicional `if-else`.
9. O programa finaliza sua execução.

Esse código cria uma lista de números de 1 a 100, filtra apenas os números pares, calcula a média desses números e verifica se a lista possui algum número primo. Em seguida, imprime a média dos números pares e se a lista possui ou não números primos.