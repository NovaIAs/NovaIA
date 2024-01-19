```portuol
procedimento ordenaVetor(inteiro vetor[], inteiro n) {
    para i = 0 até n-2 faça {
        para j = i+1 até n-1 faça {
            se vetor[j] < vetor[i] então {
                troca = vetor[i];
                vetor[i] = vetor[j];
                vetor[j] = troca;
            }
        }
    }
}

procedimento main {
    inteiro vetor[] = {10, 5, 15, 2, 8, 20};
    inteiro n = 6;

    ordenaVetor(vetor, n);

    escreva("Vetor ordenado:\n");
    para i = 0 até n-1 faça {
        escreva(vetor[i], " ");
    }
    escreva(vetor[n-1]);
}
```

Este programa ordena um vetor de números inteiros em ordem crescente usando o algoritmo de ordenação por seleção. O algoritmo funciona da seguinte forma:

1. O programa começa percorrendo o vetor do início ao fim, comparando cada elemento com todos os elementos que vêm depois dele.
2. Se um elemento for menor que o elemento anterior, os dois elementos são trocados.
3. O programa continua percorrendo o vetor até que todos os elementos estejam em ordem crescente.

No exemplo fornecido, o programa ordena o vetor `vetor` que contém os números `{10, 5, 15, 2, 8, 20}`. Após a ordenação, o vetor fica com os elementos em ordem crescente: `{2, 5, 8, 10, 15, 20}`.

O programa utiliza dois procedimentos: `ordenaVetor` e `main`. O procedimento `ordenaVetor` recebe um vetor de inteiros `vetor` e o número de elementos do vetor `n` como parâmetros e ordena o vetor em ordem crescente. O procedimento `main` é o ponto de entrada do programa e cria o vetor `vetor`, chama o procedimento `ordenaVetor` para ordenar o vetor e, por fim, exibe o vetor ordenado na tela.