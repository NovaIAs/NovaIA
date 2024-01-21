```portuguol
INÍCIO
    VARIÁVEIS
        NUMERO, POTÊNCIA, CONTADOR, RESULTADO: INTEIRO
    FIM VARIÁVEIS

    ESCREVER "DIGITE UM NÚMERO NATURL: "
    LER NUMERO

    POTÊNCIA <- 1
    CONTADOR <- 0

    ENQUANTO CONTADOR < NUMERO FAÇA
        RESULTADO <- RESULTADO + POTÊNCIA
        POTÊNCIA <- POTÊNCIA * 2
        CONTADOR <- CONTADOR + 1
    FIM ENQUANTO

    ESCREVER RESULTADO

FIM
```

**Explicação:**

1. Variáveis:
    * `NUMERO`: Armazena o número natural inserido pelo utilizador.
    * `POTÊNCIA`: Armazena o valor da potência atual, começando em 1.
    * `CONTADOR`: Conta o número de iterações do ciclo.
    * `RESULTADO`: Armazena o resultado final da soma das potências de 2.

2. Entrada do utilizador:
    * O utilizador é solicitado a digitar um número natural, que é armazenado na variável `NUMERO`.

3. Loop while:
    * O ciclo while é usado para calcular as potências de 2 e somá-las ao resultado final.
    * O ciclo é executado enquanto o contador for menor que o número natural digitado pelo utilizador.
    * A variável `RESULTADO` é atualizada em cada iteração, somando o valor da potência atual.
    * A variável `POTÊNCIA` é atualizada em cada iteração, multiplicando o seu valor por 2.
    * A variável `CONTADOR` é incrementada em cada iteração.

4. Resultado final:
    * Após o ciclo while, o resultado final é exibido na tela.