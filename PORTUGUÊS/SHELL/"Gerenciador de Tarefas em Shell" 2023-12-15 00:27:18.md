Claro! Aqui está um código complexo em Shell que implementa um sistema de gerenciamento de tarefas estilo "To-Do List". 

```shell
#!/bin/bash

# Função para adicionar uma nova tarefa
adicionar_tarefa() {
    echo "Digite o nome da tarefa:"
    read nome_tarefa
    
    echo "Digite a descrição da tarefa:"
    read descricao_tarefa
    
    echo "Digite a data limite (DD/MM/AAAA) para a conclusão da tarefa:"
    read data_limite
    
    echo "$nome_tarefa|$descricao_tarefa|$data_limite" >> tarefas.txt
    
    echo "Tarefa adicionada com sucesso!"
}

# Função para listar todas as tarefas
listar_tarefas() {
    if [ -s tarefas.txt ]; then
        while IFS='|' read -r nome_tarefa descricao_tarefa data_limite; do
            echo "-------------------------"
            echo "Nome: $nome_tarefa"
            echo "Descrição: $descricao_tarefa"
            echo "Data Limite: $data_limite"
            echo "-------------------------"
        done < tarefas.txt
    else
        echo "Não há tarefas cadastradas."
    fi
}

# Função para excluir uma tarefa
excluir_tarefa() {
    echo "Digite o número da tarefa que deseja excluir:"
    read numero_tarefa
    
    sed -i "${numero_tarefa}d" tarefas.txt
    
    echo "Tarefa excluída com sucesso!"
}

# Função para exibir o menu principal
exibir_menu() {
    echo "----- Gerenciador de Tarefas -----"
    echo "1. Adicionar nova tarefa"
    echo "2. Listar todas as tarefas"
    echo "3. Excluir uma tarefa"
    echo "4. Sair"
    echo "----------------------------------"
    echo "Digite o número da opção desejada:"
    read opcao
    
    case $opcao in
        1) adicionar_tarefa ;;
        2) listar_tarefas ;;
        3) excluir_tarefa ;;
        4) exit ;;
        *) echo "Opção inválida. Por favor, tente novamente." ;;
    esac
    
    echo ""
    exibir_menu
}

# Verifica se o arquivo de tarefas existe. Caso não exista, cria o arquivo.
if [ ! -f tarefas.txt ]; then
    touch tarefas.txt
fi

# Execução do programa
exibir_menu
```

Este código em Shell implementa um sistema de gerenciamento de tarefas simples. Ele utiliza um arquivo de texto chamado "tarefas.txt" para armazenar as informações de cada tarefa. Ao executar o código, um menu é exibido, permitindo ao usuário adicionar novas tarefas, listar todas as tarefas cadastradas, excluir uma tarefa específica ou sair do programa.

A função `adicionar_tarefa()` solicita ao usuário o nome, descrição e data limite da tarefa. As informações são então armazenadas no arquivo "tarefas.txt" usando o caractere "|" como separador.

A função `listar_tarefas()` lê o arquivo "tarefas.txt" e exibe as informações de cada tarefa cadastrada.

A função `excluir_tarefa()` solicita ao usuário o número da tarefa que deseja excluir e remove a linha correspondente do arquivo "tarefas.txt" usando o comando `sed`.

A função `exibir_menu()` exibe as opções do menu principal e realiza a chamada para as respectivas funções de acordo com a opção escolhida pelo usuário. Após executar a opção escolhida, o menu é exibido novamente.

Antes de exibir o menu principal, o código verifica se o arquivo "tarefas.txt" existe. Caso não exista, ele cria o arquivo utilizando o comando `touch`.

Espero que este código atenda às suas necessidades! Se tiver alguma dúvida, fique à vontade para perguntar.