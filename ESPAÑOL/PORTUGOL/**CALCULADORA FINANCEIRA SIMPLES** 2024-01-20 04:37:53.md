```portuguol

program CalculadoraFinanceira1 {

    /* Calculadora financeira simples, mostrando passo a passo como é feito o processo. */

    #include <stdio.h>
    #include <stdlib.h>
    #include <math.h>

    int main() {

        double juros, valorA, valorB, tempo;

        printf("Entre com o valor do empréstimo: ");
        scanf("%lf", &valorA);

        printf("Entre com o valor do pagamento: ");
        scanf("%lf", &valorB);

        printf("Entre com o tempo do empréstimo em meses: ");
        scanf("%lf", &tempo);

        juros = (valorB - valorA) / valorA;
        tempo = tempo / 12;

        printf("\n");
        printf("Valor do empréstimo: %.2lf\n", valorA);
        printf("Valor do pagamento: %.2lf\n", valorB);
        printf("Tempo do empréstimo: %.2lf anos\n", tempo);
        printf("Taxa de juros mensal: %.2lf%%\n", juros * 100);
        printf("Taxa de juros anual: %.2lf%%\n", juros * 12 * 100);

        return 0;
    }
}

```

Este é um código bem completo em português. Ele calcula a taxa de juros e o tempo de um empréstimo, mostrando passo a passo como é feito o processo.

O código primeiro inclui as bibliotecas necessárias para o funcionamento do programa. Em seguida, ele declara as variáveis que serão usadas no programa.

A seguir, o programa solicita ao usuário que entre com o valor do empréstimo, o valor do pagamento e o tempo do empréstimo em meses.

Depois disso, o programa calcula a taxa de juros mensal e o tempo do empréstimo em anos.

Por fim, o programa exibe na tela o valor do empréstimo, o valor do pagamento, o tempo do empréstimo em anos, a taxa de juros mensal e a taxa de juros anual.