```portuguol
INÍCIO
    VAR SOMA, CONTADOR: REAL;

    SOMA <- 0;
    CONTADOR <- 0;

    ENQUANTO CONTADOR < 10 FAÇA
        SOMA <- SOMA + CONTADOR;
        CONTADOR <- CONTADOR + 1;
    FACA

    IMPRIMA("A soma dos 10 primeiros números naturais é: ", SOMA);

    SE SOMA > 50 ENTÃO
        IMPRIMA("A soma é maior que 50");
    SENÃO
        IMPRIMA("A soma é menor ou igual a 50");
    FIM_SE

    PARA INDICE DE 1 ATÉ 5 FAÇA
        IMPRIMA("Índice atual: ", INDICE);
    FIM_PARA

    PARA CADA ELEMENTO EM [1, 3, 5, 7, 9] FAÇA
        IMPRIMA("Elemento atual: ", ELEMENTO);
    FIM_PARA

    SELECIONE
        CASO SOMA > 50:
            IMPRIMA("A soma é maior que 50");
        CASO SOMA < 50:
            IMPRIMA("A soma é menor que 50");
        CASO CONTRÁRIO:
            IMPRIMA("A soma é igual a 50");
    FIM_SELECIONE

    FUNÇÃO FATORIAL(NÚMERO): REAL;
        SE NÚMERO = 1 ENTÃO
            RETORNE 1;
        SENÃO
            RETORNE NÚMERO * FATORIAL(NÚMERO - 1);
        FIM_SE
    FIM_FUNÇÃO

    IMPRIMA("Fatorial de 5: ", FATORIAL(5));

    RETORNE 0;
FIM

```