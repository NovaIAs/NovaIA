Claro! Aqui está um exemplo de um código complexo em Python que envolve a criação de uma calculadora com funcionalidades avançadas:

```python
# Importando a biblioteca math
import math

# Função para realizar as operações básicas da calculadora
def calcular():
    print("Bem-vindo à calculadora avançada!")
    print("Escolha uma das opções abaixo:")
    print("1 - Soma")
    print("2 - Subtração")
    print("3 - Multiplicação")
    print("4 - Divisão")
    print("5 - Potenciação")
    print("6 - Raiz quadrada")
    print("0 - Sair")

    opcao = int(input("Digite o número da opção desejada: "))

    if opcao == 0:
        print("Calculadora encerrada.")
    elif opcao >= 1 and opcao <= 4:
        num1 = float(input("Digite o primeiro número: "))
        num2 = float(input("Digite o segundo número: "))

        if opcao == 1:
            resultado = num1 + num2
            print("O resultado da soma é:", resultado)
        elif opcao == 2:
            resultado = num1 - num2
            print("O resultado da subtração é:", resultado)
        elif opcao == 3:
            resultado = num1 * num2
            print("O resultado da multiplicação é:", resultado)
        else:
            if num2 != 0:
                resultado = num1 / num2
                print("O resultado da divisão é:", resultado)
            else:
                print("Erro: divisão por zero.")

    elif opcao == 5:
        num1 = float(input("Digite o número: "))
        num2 = float(input("Digite o expoente: "))
        resultado = math.pow(num1, num2)
        print("O resultado da potenciação é:", resultado)

    elif opcao == 6:
        num = float(input("Digite o número: "))
        resultado = math.sqrt(num)
        print("O resultado da raiz quadrada é:", resultado)

    else:
        print("Opção inválida.")

    print("\n")
    calcular()  # Chamada recursiva da função para continuar calculando


calcular()  # Chama a função calcular para iniciar a calculadora
```

Neste código, importamos a biblioteca math para utilizar funções matemáticas avançadas, como a potenciação e a raiz quadrada. Em seguida, definimos a função `calcular()` que exibe um menu de opções para o usuário. Dependendo da opção escolhida, o código realiza as operações matemáticas correspondentes e exibe o resultado. 

Além disso, o código também inclui validações para evitar erros, como a divisão por zero. Para sair da calculadora, basta digitar 0. A função `calcular()` é chamada de forma recursiva para permitir que o usuário realize várias operações consecutivas.

Espero que este código atenda às suas expectativas!