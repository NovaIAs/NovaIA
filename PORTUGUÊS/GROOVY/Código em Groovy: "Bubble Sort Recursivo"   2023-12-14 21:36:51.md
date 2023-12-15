Claro! Aqui está um código complexo em Groovy, que implementa um algoritmo de ordenação chamado "Bubble Sort" de forma recursiva. O Bubble Sort é um algoritmo de ordenação simples, porém ineficiente, que compara repetidamente pares de elementos adjacentes e os troca se estiverem na ordem errada.

```groovy
def bubbleSortRecursivo(list) {
    def n = list.size()
    if (n <= 1) {
        return list
    }
    def trocou = false
    for (def i = 0; i < n - 1; i++) {
        if (list[i] > list[i + 1]) {
            def temp = list[i]
            list[i] = list[i + 1]
            list[i + 1] = temp
            trocou = true
        }
    }
    if (trocou) {
        return bubbleSortRecursivo(list.take(n - 1)) + [list[n - 1]]
    }
    return list
}

def listaDesordenada = [5, 2, 8, 12, 1, 9, 3]
def listaOrdenada = bubbleSortRecursivo(listaDesordenada)

println "Lista desordenada: $listaDesordenada"
println "Lista ordenada: $listaOrdenada"
```

Explicação do código:

1. A função `bubbleSortRecursivo` recebe uma lista como parâmetro e retorna a lista ordenada.
2. A variável `n` armazena o tamanho da lista.
3. Se a lista tiver tamanho menor ou igual a 1, ela já está ordenada, então retornamos a lista.
4. A variável `trocou` é inicializada como `false`, indicando que ainda não houve trocas.
5. Iniciamos um loop `for`, percorrendo a lista até o penúltimo elemento.
6. Dentro do loop, comparamos cada elemento com o próximo. Se estiverem fora de ordem, trocamos os elementos de posição e marcamos `trocou` como `true`.
7. Se ocorreram trocas durante o loop, chamamos recursivamente a função `bubbleSortRecursivo` passando a lista sem o último elemento (`list.take(n - 1)`) e concatenamos com o último elemento da lista original (`[list[n - 1]]`).
8. Se não ocorreram trocas, a lista está ordenada e retornamos a lista original.
9. Na parte final do código, criamos uma lista desordenada para testar o algoritmo e chamamos a função `bubbleSortRecursivo` para ordenar a lista.
10. Por fim, imprimimos a lista desordenada e a lista ordenada.

Espero que este código atenda às suas expectativas! Lembre-se de que o Bubble Sort é um algoritmo ineficiente para grandes quantidades de dados, mas é útil para fins educacionais e de aprendizado.