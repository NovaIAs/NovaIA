**Code complexe en Portugol :**

```portugol
programa ManipulacaoComplexa {

    // Variáveis
    inteiro matriz[5,5], vetor[10], num, i, j, cont;
    real media;

    // Entrada da matriz
    para i de 1 ate 5 faca
        para j de 1 ate 5 faca
            escreva("Digite o valor da matriz na posição ", i, ", ", j, ": ");
            leia(matriz[i,j]);
        fimpara
    fimpara

    // Entrada do vetor
    para i de 1 ate 10 faca
        escreva("Digite o valor do vetor na posição ", i, ": ");
        leia(vetor[i]);
    fimpara

    // Entrada do número
    escreva("Digite um número: ");
    leia(num);

    // Manipulações dos dados

    // Soma dos elementos da matriz principal
    inteiro somaPrincipal = 0;
    para i de 1 ate 5 faca
        para j de 1 ate 5 faca
            se (i == j) entao
                somaPrincipal += matriz[i,j];
            fimse
        fimpara
    fimpara

    // Média dos elementos acima da diagonal secundária
    real somaSecundaria = 0;
    cont = 0;
    para i de 1 ate 5 faca
        para j de 1 ate i faca
            se (j < i) entao
                somaSecundaria += matriz[i,j];
                cont += 1;
            fimse
        fimpara
    fimpara
    media = somaSecundaria / cont;

    // Contagem de elementos pares no vetor
    cont = 0;
    para i de 1 ate 10 faca
        se (vetor[i] % 2 == 0) entao
            cont += 1;
        fimse
    fimpara

    // Verificação se o número é ímpar ou par
    se (num % 2 == 0) entao
        escreva("O número ", num, " é par.");
    senao
        escreva("O número ", num, " é ímpar.");
    fimse

    // Exibição dos resultados
    escreva("\nResultados:");
    escreva("Soma dos elementos da diagonal principal da matriz: ", somaPrincipal);
    escreva("Média dos elementos acima da diagonal secundária da matriz: ", media);
    escreva("Número de elementos pares no vetor: ", cont);

}
```

**Explicação do código:**

* **Entrada dos dados:** O código solicita ao usuário que insira valores para uma matriz 5x5, um vetor de 10 elementos e um número inteiro.

* **Manipulações da matriz:**
    * **Soma dos elementos da diagonal principal:** Ele calcula a soma dos elementos localizados na diagonal principal da matriz, ou seja, da posição (1,1) até (5,5).
    * **Média dos elementos acima da diagonal secundária:** Ele calcula a média dos elementos localizados acima da diagonal secundária da matriz, ou seja, da posição (2,1) até (5,4).

* **Manipulações do vetor:**
    * **Contagem de elementos pares:** Ele conta o número de elementos pares no vetor.

* **Manipulações do número:**
    * **Verificação de paridade:** Ele verifica se o número inserido pelo usuário é par ou ímpar.

* **Exibição dos resultados:** O código exibe os resultados das manipulações realizadas, incluindo a soma da diagonal principal, a média dos elementos acima da diagonal secundária, o número de elementos pares no vetor e a classificação do número como par ou ímpar.