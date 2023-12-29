Claro! Vou gerar um código complexo em Python para você. Este código será um sistema de gerenciamento de tarefas que permitirá ao usuário adicionar, excluir, atualizar e visualizar tarefas. Ele usará uma lista para armazenar as tarefas e um loop para possibilitar a interação contínua com o usuário.

Aqui está o código:

```python
# Função para adicionar uma tarefa
def adicionar_tarefa(tarefas):
    tarefa = input("Digite a tarefa a ser adicionada: ")
    tarefas.append(tarefa)
    print("Tarefa adicionada com sucesso!")

# Função para excluir uma tarefa
def excluir_tarefa(tarefas):
    tarefa = input("Digite a tarefa a ser excluída: ")
    if tarefa in tarefas:
        tarefas.remove(tarefa)
        print("Tarefa excluída com sucesso!")
    else:
        print("Tarefa não encontrada!")

# Função para atualizar uma tarefa
def atualizar_tarefa(tarefas):
    tarefa_antiga = input("Digite a tarefa a ser atualizada: ")
    if tarefa_antiga in tarefas:
        indice = tarefas.index(tarefa_antiga)
        tarefa_nova = input("Digite a nova tarefa: ")
        tarefas[indice] = tarefa_nova
        print("Tarefa atualizada com sucesso!")
    else:
        print("Tarefa não encontrada!")

# Função para visualizar todas as tarefas
def visualizar_tarefas(tarefas):
    if len(tarefas) > 0:
        print("Lista de tarefas:")
        for tarefa in tarefas:
            print("- " + tarefa)
    else:
        print("Nenhuma tarefa encontrada!")

# Lista para armazenar as tarefas
tarefas = []

# Loop principal
while True:
    print("\n=== Sistema de Gerenciamento de Tarefas ===")
    print("Selecione uma opção:")
    print("1 - Adicionar tarefa")
    print("2 - Excluir tarefa")
    print("3 - Atualizar tarefa")
    print("4 - Visualizar tarefas")
    print("5 - Sair")

    opcao = input("Digite o número da opção desejada: ")

    if opcao == "1":
        adicionar_tarefa(tarefas)
    elif opcao == "2":
        excluir_tarefa(tarefas)
    elif opcao == "3":
        atualizar_tarefa(tarefas)
    elif opcao == "4":
        visualizar_tarefas(tarefas)
    elif opcao == "5":
        break
    else:
        print("Opção inválida! Por favor, tente novamente.")
```

Neste código, as funções `adicionar_tarefa`, `excluir_tarefa`, `atualizar_tarefa` e `visualizar_tarefas` são responsáveis por realizar as operações correspondentes. A lista `tarefas` é usada para armazenar as tarefas adicionadas pelo usuário.

Dentro do loop principal, o usuário pode selecionar uma opção digitando o número correspondente. As funções são chamadas de acordo com a opção selecionada, permitindo que o usuário gerencie suas tarefas adicionando, excluindo, atualizando ou visualizando-as. O loop continua até que o usuário escolha a opção "5", que encerra o programa.

Espero que este código atenda às suas expectativas! Lembre-se de adaptá-lo caso necessário.