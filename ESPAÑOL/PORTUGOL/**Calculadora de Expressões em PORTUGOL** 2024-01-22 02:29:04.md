```portugol

programa CalculadoraDeExpressoes

    # Declaração de variáveis
    real x, y, resultado

    # Leitura de valores
    leia(x)
    leia(y)

    # Seleção de operação
    escreva("Escolha uma operação: \n")
    escreva("1. Soma \n")
    escreva("2. Subtração \n")
    escreva("3. Multiplicação \n")
    escreva("4. Divisão \n")
    leia(opcao)

    # Realização da operação selecionada
    se (opcao == 1) entao
        resultado = x + y
    fim-se
    senao-se (opcao == 2) entao
        resultado = x - y
    fim-senao-se
    senao-se (opcao == 3) entao
        resultado = x * y
    fim-senao-se
    senao-se (opcao == 4) entao
        se (y != 0) entao
            resultado = x / y
        senao
            escreva("Não é possível dividir por 0. \n")
        fim-se
    fim-senao-se

    # Exibição do resultado
    escreva("O resultado da operação é: ", resultado)

fim-programa

```

O código que escrevi é uma calculadora simples em PORTUGOL. Ele permite ao usuário escolher uma operação entre soma, subtração, multiplicação e divisão e, em seguida, insere dois valores para realizar a operação.

O programa usa uma estrutura de seleção para escolher a operação a ser realizada. Em seguida, ele usa uma estrutura de caso para realizar a operação selecionada. O código também usa uma estrutura de condicional para verificar se o divisor é zero antes de realizar a divisão.

O programa também usa a função `leia()` para ler os valores do usuário e a função `escreva()` para exibir o resultado da operação.

Este código é relativamente complexo porque usa várias estruturas de controle diferentes e também usa funções para ler e escrever valores. No entanto, o código é bem organizado e fácil de entender.