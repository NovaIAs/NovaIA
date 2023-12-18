Claro! Aqui está um exemplo de código em PORTUGOL que implementa um algoritmo de ordenação conhecido como Merge Sort:

```
Algoritmo MergeSort

Função Merge(vetor, inicio, meio, fim)
    tamanhoEsquerda = meio - inicio + 1
    tamanhoDireita = fim - meio
    
    // Criação de vetores temporários para armazenar as duas metades do vetor original
    vetorEsquerda = novo vetor[tamanhoEsquerda]
    vetorDireita = novo vetor[tamanhoDireita]
    
    // Copia os elementos para os vetores temporários
    para i = 0 até tamanhoEsquerda - 1 faça
        vetorEsquerda[i] = vetor[inicio + i]
    fim para
    
    para j = 0 até tamanhoDireita - 1 faça
        vetorDireita[j] = vetor[meio + 1 + j]
    fim para
    
    // Mescla os dois vetores temporários de volta para o vetor original
    i = 0
    j = 0
    k = inicio
    
    enquanto i < tamanhoEsquerda e j < tamanhoDireita faça
        se vetorEsquerda[i] <= vetorDireita[j] então
            vetor[k] = vetorEsquerda[i]
            i = i + 1
        senão
            vetor[k] = vetorDireita[j]
            j = j + 1
        fim se
        k = k + 1
    fim enquanto
    
    // Copia os elementos restantes do vetor esquerdo (se houver algum)
    enquanto i < tamanhoEsquerda faça
        vetor[k] = vetorEsquerda[i]
        i = i + 1
        k = k + 1
    fim enquanto
    
    // Copia os elementos restantes do vetor direito (se houver algum)
    enquanto j < tamanhoDireita faça
        vetor[k] = vetorDireita[j]
        j = j + 1
        k = k + 1
    fim enquanto
    
Fim Função

Procedimento OrdenarMergeSort(vetor, inicio, fim)
    se inicio < fim então
        meio = (inicio + fim) / 2
        
        // Ordena recursivamente as duas metades do vetor
        OrdenarMergeSort(vetor, inicio, meio)
        OrdenarMergeSort(vetor, meio + 1, fim)
        
        // Mescla as duas metades ordenadas do vetor
        Merge(vetor, inicio, meio, fim)
    fim se

Fim Procedimento

Algoritmo Principal

vetor = [5, 8, 1, 3, 6, 2, 7, 4]
tamanhoVetor = tamanho(vetor)

Escreva("Vetor original: ")
para i = 0 até tamanhoVetor - 1 faça
    Escreva(vetor[i], " ")
fim para

Escreva("\n")

OrdenarMergeSort(vetor, 0, tamanhoVetor - 1)

Escreva("Vetor ordenado usando Merge Sort: ")
para i = 0 até tamanhoVetor - 1 faça
    Escreva(vetor[i], " ")
fim para

Fim Algoritmo
```

Neste código, o algoritmo Merge Sort é implementado para ordenar um vetor de números inteiros. Ele utiliza uma abordagem de divisão e conquista, dividindo o vetor em duas metades até que cada subvetor tenha apenas um elemento. Em seguida, os subvetores são mesclados de forma ordenada, resultando no vetor original completamente ordenado. O código mostra como chamar a função `OrdenarMergeSort` para ordenar um vetor específico e exibe o vetor original e o vetor ordenado como saída.