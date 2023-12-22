Claro! Aqui está um exemplo de código complexo em LUA, que implementa um algoritmo de classificação chamado Bubble Sort:

```lua
-- Função para imprimir o vetor
function imprimirVetor(vetor)
    local tamanho = #vetor
    for i = 1, tamanho do
        io.write(vetor[i].." ")
    end
    io.write("\n")
end

-- Função para realizar o Bubble Sort
function bubbleSort(vetor)
    local tamanho = #vetor
    for i = 1, tamanho - 1 do
        for j = 1, tamanho - i do
            if vetor[j] > vetor[j + 1] then
                vetor[j], vetor[j + 1] = vetor[j + 1], vetor[j]
            end
        end
    end
end

-- Função principal
function main()
    local vetor = {9, 3, 6, 2, 7, 1, 5, 4, 8}
    io.write("Vetor original: ")
    imprimirVetor(vetor)
    
    bubbleSort(vetor)
    
    io.write("Vetor ordenado: ")
    imprimirVetor(vetor)
end

-- Chamada da função principal
main()
```

Neste código, temos três funções principais: `imprimirVetor`, `bubbleSort` e `main`.

A função `imprimirVetor` recebe um vetor como parâmetro e imprime seus elementos separados por espaço. Isso é feito usando um loop `for` e a função `io.write`.

A função `bubbleSort` implementa o algoritmo de classificação Bubble Sort. Ela recebe um vetor como parâmetro e realiza as trocas necessárias para ordenar o vetor em ordem crescente. O algoritmo utiliza dois loops `for` para percorrer o vetor e comparar elementos adjacentes. Caso o elemento atual seja maior que o próximo, ocorre a troca dos elementos.

A função `main` é a função principal do programa. Ela cria um vetor de exemplo, imprime o vetor original, chama a função `bubbleSort` para ordenar o vetor e, por fim, imprime o vetor ordenado.

Portanto, ao executar esse código em LUA, você verá a saída impressa do vetor original e do vetor ordenado após a aplicação do algoritmo Bubble Sort.