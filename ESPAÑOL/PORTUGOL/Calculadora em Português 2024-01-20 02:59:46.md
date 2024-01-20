```portugol
procedimento principal()
inicio

inteiro opcao, valor1, valor2, resultado

repetir
      escreva("\nMENU:\n")
      escreva("-----------------\n")
      escreva("1. Adição\n")
      escreva("2. Subtração\n")
      escreva("3. Multiplicação\n")
      escreva("4. Divisão\n")
      escreva("5. Potênciação\n")
      escreva("6. Raiz quadrada\n")
      escreva("0. Sair\n")
      escreva("-----------------\n")
      escreva("Digite a opção desejada: ")
      leia(opcao)

      escolha opcao
      caso 1:
            escreva("Digite o primeiro valor: ")
            leia(valor1)
            escreva("Digite o segundo valor: ")
            leia(valor2)
            resultado := valor1 + valor2
            escreva("A soma dos valores é: ", resultado)
      caso 2:
            escreva("Digite o primeiro valor: ")
            leia(valor1)
            escreva("Digite o segundo valor: ")
            leia(valor2)
            resultado := valor1 - valor2
            escreva("A diferença dos valores é: ", resultado)
      caso 3:
            escreva("Digite o primeiro valor: ")
            leia(valor1)
            escreva("Digite o segundo valor: ")
            leia(valor2)
            resultado := valor1 * valor2
            escreva("O produto dos valores é: ", resultado)
      caso 4:
            escreva("Digite o primeiro valor: ")
            leia(valor1)
            escreva("Digite o segundo valor: ")
            leia(valor2)
            resultado := valor1 / valor2
            escreva("O quociente dos valores é: ", resultado)
      caso 5:
            escreva("Digite o primeiro valor: ")
            leia(valor1)
            escreva("Digite o segundo valor: ")
            leia(valor2)
            resultado := valor1 ^ valor2
            escreva("A potência do primeiro valor elevado ao segundo é: ", resultado)
      caso 6:
            escreva("Digite o valor: ")
            leia(valor1)
            resultado := sqrt(valor1)
            escreva("A raiz quadrada do valor é: ", resultado)
      caso 0:
            finalizar
      padrao;
            escreva("Opção inválida!")
      fim_escolha

ate opcao <> 0

fim_procedimento

procedimento finalizar()
inicio
    escreva("\nObrigado por usar a calculadora!")
    encerrar
fim_procedimento
```

Este código é uma calculadora simples em português que permite ao usuário escolher entre uma variedade de operações matemáticas, incluindo adição, subtração, multiplicação, divisão, potenciação e raiz quadrada. 

O código é dividido em duas partes: a primeira inicializa as variáveis, lê a entrada do usuário e exibe o menu, enquanto a segunda realiza as operações matemáticas selecionadas pelo usuário. 

Para utilizar a calculadora, o usuário deve digitar uma opção entre 1 e 6 para selecionar a operação desejada. Em seguida, o usuário deverá digitar os valores que deseja utilizar na operação. 

A calculadora então realizará a operação e exibirá o resultado. Se o usuário digitar uma opção inválida, será exibida uma mensagem de erro. O usuário pode sair da calculadora a qualquer momento digitando a opção 0.