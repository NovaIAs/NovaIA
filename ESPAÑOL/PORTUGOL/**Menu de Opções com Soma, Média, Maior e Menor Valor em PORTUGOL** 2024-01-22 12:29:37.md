```PORTUGOL
procedimento Inicio()
     inteiro opção, cont
     cadeia str
     real valores_numeros[10]
     real médias[3]
     real valor_total, média_geral
     
     escrever("1. Suma de valores")
     escrever("2. Média de valores")
     escrever("3. Maior e menor valor")
     escrever("4. Sair")
     opção = ler_inteiro()
     escrever()
     
     enquanto opção != 4 fazer
          caso opção de
               1 :
                    escrever("Quantidade de valores a serem somados:")
                    cont = ler_inteiro()
                    valor_total = 0
                    para i = 1 até cont fazer
                         escrever("Valor ", i, ": ")
                         valores_numeros[i] = ler_real()
                         valor_total = valor_total + valores_numeros[i]
                    fim_para
                    escrever()
                    escrever("O valor total da soma é ", valor_total)
                    escrever()
               fim_caso
               
               2 :
                    escrever("Quantidade de valores para se calcular a média:")
                    cont = ler_inteiro()
                    valor_total = 0
                    para i = 1 até cont fazer
                         escrever("Valor ", i, ": ")
                         valores_numeros[i] = ler_real()
                         valor_total = valor_total + valores_numeros[i]
                    fim_para
                    escrever()
                    média_geral = valor_total / cont
                    escrever("A média dos valores é ", média_geral)
                    escrever()
               fim_caso
               
               3 :
                    escrever("Quantidade de valores para se encontrar o maior e o menor:")
                    cont = ler_inteiro()
                    para i = 1 até cont fazer
                         escrever("Valor ", i, ": ")
                         valores_numeros[i] = ler_real()
                    fim_para
                    escrever()
                    maior_valor = valores_numeros[1]
                    menor_valor = valores_numeros[1]
                    para i = 2 até cont fazer
                         se valores_numeros[i] > maior_valor então
                              maior_valor = valores_numeros[i]
                         fim_se
                         se valores_numeros[i] < menor_valor então
                              menor_valor = valores_numeros[i]
                         fim_se
                    fim_para
                    escrever("O maior valor é ", maior_valor)
                    escrever("O menor valor é ", menor_valor)
                    escrever()
               fim_caso
          fim_caso
          
          escrever("1. Suma de valores")
          escrever("2. Média de valores")
          escrever("3. Maior e menor valor")
          escrever("4. Sair")
          opção = ler_inteiro()
          escrever()
     fim_enquanto
fim_procedimento

Inicio()
```

Explicação do código:

O código apresentado é um programa escrito na linguagem de programação PORTUGOL. Ele apresenta um menu de opções ao usuário, permitindo que ele escolha entre quatro operações: soma de valores, média de valores, maior e menor valor e sair.

A primeira opção permite ao usuário somar uma sequência de valores. O usuário é solicitado a informar a quantidade de valores a serem somados e, em seguida, é solicitado a informar cada valor individualmente. O programa então calcula a soma dos valores informados e exibe o resultado na tela.

A segunda opção permite ao usuário calcular a média de uma sequência de valores. O usuário é solicitado a informar a quantidade de valores para os quais deseja calcular a média e, em seguida, é solicitado a informar cada valor individualmente. O programa então calcula a média dos valores informados e exibe o resultado na tela.

A terceira opção permite ao usuário encontrar o maior e o menor valor de uma sequência de valores. O usuário é solicitado a informar a quantidade de valores para os quais deseja encontrar o maior e o menor valor e, em seguida, é solicitado a informar cada valor individualmente. O programa então encontra o maior e o menor valor entre os valores informados e exibe o resultado na tela.

A quarta opção permite ao usuário sair do programa.

O programa utiliza a estrutura de seleção encadeada (caso) para implementar o menu de opções. A variável `opção` é utilizada para armazenar a opção selecionada pelo usuário. O programa utiliza um loop `enquanto` para repetir o menu de opções até que o usuário escolha a opção de sair.

O programa também utiliza vetores para armazenar os valores informados pelo usuário. Os vetores `valores_numeros` e `médias` são utilizados para armazenar os valores informados pelo usuário e as médias calculadas pelo programa, respectivamente.

O programa utiliza as funções `escrever()` e `ler_inteiro()` para exibir mensagens na tela e ler valores inteiros do teclado, respectivamente. O programa também utiliza as funções `ler_real()` e `ler_cadeia()` para ler valores reais e cadeias de caracteres do teclado, respectivamente.