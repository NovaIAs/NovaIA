```portuguol
INICIO

  # Declaração de Variáveis
  REAL valor1, valor2, resultadoSoma, resultadoSubtração, resultadoMultiplicação, resultadoDivisão, expoente, resultadoPotencia
  CARACTER opção

  # Cabeçalho do Programa
  ESCREVER "Calculadora Completa em Portugol"

  # Loop para o Menu Principal
  FAZER
    # Exibe o Menu Principal
    ESCREVER
    ESCREVER "--------------------------------------------------"
    ESCREVER "1 - Soma"
    ESCREVER "2 - Subtração"
    ESCREVER "3 - Multiplicação"
    ESCREVER "4 - Divisão"
    ESCREVER "5 - Potência"
    ESCREVER "6 - Sair"
    ESCREVER "--------------------------------------------------"
    ESCREVER "Opção: "

    # Lê a Opção Escolhida pelo Usuário
    LER opção

    # Verifica a Opção Escolhida pelo Usuário
    SE opção = "1" ENTÃO
      # Soma
      ESCREVER "Digite o Primeiro Valor: "
      LER valor1
      ESCREVER "Digite o Segundo Valor: "
      LER valor2
      resultadoSoma = valor1 + valor2
      ESCREVER "Resultado da Soma: " << resultadoSoma
    SENÃO SE opção = "2" ENTÃO
      # Subtração
      ESCREVER "Digite o Primeiro Valor: "
      LER valor1
      ESCREVER "Digite o Segundo Valor: "
      LER valor2
      resultadoSubtração = valor1 - valor2
      ESCREVER "Resultado da Subtração: " << resultadoSubtração
    SENÃO SE opção = "3" ENTÃO
      # Multiplicação
      ESCREVER "Digite o Primeiro Valor: "
      LER valor1
      ESCREVER "Digite o Segundo Valor: "
      LER valor2
      resultadoMultiplicação = valor1 * valor2
      ESCREVER "Resultado da Multiplicação: " << resultadoMultiplicação
    SENÃO SE opção = "4" ENTÃO
      # Divisão
      ESCREVER "Digite o Primeiro Valor: "
      LER valor1
      ESCREVER "Digite o Segundo Valor: "
      LER valor2
      SE valor2 <> 0 ENTÃO
        resultadoDivisão = valor1 / valor2
        ESCREVER "Resultado da Divisão: " << resultadoDivisão
      SENÃO
        ESCREVER "Não é possível dividir por zero."
      FIM SE
    SENÃO SE opção = "5" ENTÃO
      # Potência
      ESCREVER "Digite o Valor da Base: "
      LER valor1
      ESCREVER "Digite o Valor do Expoente: "
      LER expoente
      resultadoPotencia = valor1 ** expoente
      ESCREVER "Resultado da Potência: " << resultadoPotencia
    SENÃO SE opção = "6" ENTÃO
      # Sair do Programa
      ESCREVER "Obrigado por usar a Calculadora Completa em Portugol!"
      SAIR
    FIM SE

  ENQUANTO opção <> "6"

FIM
```

**Explicação do Código**:

1. O programa começa com a declaração de variáveis, que serão utilizadas para armazenar os valores dos números a serem calculados e os resultados das operações.

2. Em seguida, o programa exibe um cabeçalho para o programa, com o título "Calculadora Completa em Portugol".

3. O programa entra em um loop principal, que se repete enquanto o usuário não escolher a opção de sair.

4. Dentro do loop principal, o programa exibe um menu com as opções de operações disponíveis: soma, subtração, multiplicação, divisão e potência.

5. O usuário é solicitado a escolher uma opção, digitando o número correspondente à operação desejada.

6. Dependendo da opção escolhida pelo usuário, o programa solicita que o usuário digite os valores dos números a serem calculados.

7. O programa então executa a operação selecionada e exibe o resultado da operação.

8. Se o usuário escolher a opção de sair, o programa exibe uma mensagem de agradecimento e sai do programa.

No código, são utilizadas as seguintes instruções do Portugol:

* `INICIO`: Inicia o programa.
* `FIM`: Finaliza o programa.
* `ESCREVER`: Exibe uma mensagem na tela.
* `LER`: Lê um valor digitado pelo usuário.
* `FAZER`: Inicia um loop.
* `ENQUANTO`: Continua o loop enquanto uma condição for verdadeira.
* `SE`: Executa um bloco de código se uma condição for verdadeira.
* `SENÃO SE`: Executa um bloco de código se uma condição for verdadeira, caso contrário, executa outro bloco de código.
* `FIM SE`: Finaliza um bloco `SE` ou `SENÃO SE`.
* `SAIR`: Sai do programa.