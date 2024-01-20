```portuguol
# Programa que simula um caixa eletrônico
# Declaração de variáveis
saldo = 1000.0  # Saldo inicial da conta
saque = 0.0  # Valor do saque
deposito = 0.0  # Valor do depósito

# Loop principal do programa
while True:
    # Mostra o menu de opções para o usuário
    print("--------------------------------------------------")
    print("1 - Saldo")
    print("2 - Saque")
    print("3 - Depósito")
    print("4 - Sair")
    print("--------------------------------------------------")

    # Lê a opção escolhida pelo usuário
    opcao = int(input("Digite a opção desejada: "))

    # Verifica a opção escolhida pelo usuário
    if opcao == 1:
        # Mostra o saldo da conta para o usuário
        print(f"Seu saldo é R${saldo}")
    elif opcao == 2:
        # Lê o valor do saque
        saque = float(input("Digite o valor do saque: "))

        # Verifica se o valor do saque é válido
        if saque <= saldo:
            # Atualiza o saldo da conta
            saldo -= saque

            # Mostra o novo saldo da conta para o usuário
            print(f"Seu novo saldo é R${saldo}")
        else:
            # Mostra uma mensagem de erro para o usuário
            print("Saque inválido. O valor do saque não pode ser maior que o saldo da conta.")
    elif opcao == 3:
        # Lê o valor do depósito
        deposito = float(input("Digite o valor do depósito: "))

        # Atualiza o saldo da conta
        saldo += deposito

        # Mostra o novo saldo da conta para o usuário
        print(f"Seu novo saldo é R${saldo}")
    elif opcao == 4:
        # Sai do loop principal do programa
        break
    else:
        # Mostra uma mensagem de erro para o usuário
        print("Opção inválida. Digite uma opção válida.")

# Mensagem de despedida para o usuário
print("Obrigado por usar o nosso caixa eletrônico.")
```

Este código simula um caixa eletrônico. O usuário pode escolher entre as seguintes opções:

* Saldo: Mostra o saldo da conta corrente do usuário.
* Saque: Permite que o usuário saque dinheiro da conta corrente.
* Depósito: Permite que o usuário deposite dinheiro na conta corrente.
* Sair: Sai do programa.

O programa usa um loop principal para exibir o menu de opções e ler a opção escolhida pelo usuário. Dependendo da opção escolhida, o programa executa a ação correspondente.

O programa usa as seguintes variáveis:

* saldo: Armazena o saldo da conta corrente do usuário.
* saque: Armazena o valor do saque solicitado pelo usuário.
* deposito: Armazena o valor do depósito solicitado pelo usuário.

O programa usa as seguintes funções:

* print(): Exibe uma mensagem na tela.
* input(): Lê uma entrada do usuário.
* float(): Converte uma string em um número de ponto flutuante.
* int(): Converte uma string em um número inteiro.