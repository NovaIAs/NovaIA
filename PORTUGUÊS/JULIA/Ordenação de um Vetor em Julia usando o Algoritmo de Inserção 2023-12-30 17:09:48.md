Claro! Aqui está um código em Julia que faz a ordenação de um vetor usando o algoritmo de ordenação por inserção. O código é bastante extenso e está completamente em português, com explicações detalhadas em cada etapa:

```julia
function ordenacao_insercao(vetor)
    # Percorre o vetor a partir do segundo elemento
    for i in 2:length(vetor)
        chave = vetor[i]
        j = i - 1

        # Compara o elemento atual com os elementos anteriores
        while j >= 1 && vetor[j] > chave
            vetor[j+1] = vetor[j]
            j = j - 1
        end

        # Insere o elemento atual no lugar correto
        vetor[j+1] = chave
    end

    return vetor
end

# Função para imprimir o vetor ordenado
function imprimir_vetor(vetor)
    println("Vetor ordenado:")
    for elemento in vetor
        print(elemento, " ")
    end
    println()
end

# Exemplo de uso
vetor = [5, 2, 9, 3, 1, 7]
println("Vetor original:")
imprimir_vetor(vetor)

vetor_ordenado = ordenacao_insercao(vetor)
imprimir_vetor(vetor_ordenado)
```

Nesse código, a função `ordenacao_insercao` implementa o algoritmo de ordenação por inserção. Ela percorre o vetor a partir do segundo elemento e compara o elemento atual com os elementos anteriores. Se o elemento atual for menor que o elemento anterior, eles são trocados de posição. Esse processo é repetido até que o elemento atual esteja na posição correta.

A função `imprimir_vetor` é responsável por imprimir o vetor ordenado na tela. Ela percorre o vetor e imprime cada elemento separado por um espaço.

No exemplo de uso, um vetor de números inteiros é definido e impresso na tela. Em seguida, a função `ordenacao_insercao` é chamada para ordenar o vetor. Por fim, o vetor ordenado é impresso novamente.