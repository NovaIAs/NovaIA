Claro! Vou criar um código complexo em Shell para você. Este código será um programa de agenda de contatos, onde você poderá adicionar, visualizar, editar e excluir contatos. 

```shell
#!/bin/bash

# Definindo as funções

# Função para adicionar um novo contato
adicionar_contato() {
    echo "Digite o nome do contato:"
    read nome

    echo "Digite o telefone do contato:"
    read telefone

    echo "Digite o e-mail do contato:"
    read email

    echo "$nome;$telefone;$email" >> agenda.txt

    echo "Contato adicionado com sucesso!"
}

# Função para visualizar todos os contatos
visualizar_contatos() {
    if [ -s agenda.txt ]; then
        while IFS=";" read -r nome telefone email; do
            echo "Nome: $nome"
            echo "Telefone: $telefone"
            echo "E-mail: $email"
            echo "-----------------------"
        done < agenda.txt
    else
        echo "A agenda está vazia."
    fi
}

# Função para editar um contato existente
editar_contato() {
    echo "Digite o nome do contato que deseja editar:"
    read nome

    if grep -q "$nome" agenda.txt; then
        echo "Digite o novo telefone do contato:"
        read novo_telefone

        echo "Digite o novo e-mail do contato:"
        read novo_email

        sed -i "s/^$nome;.*$/$nome;$novo_telefone;$novo_email/" agenda.txt

        echo "Contato editado com sucesso!"
    else
        echo "Contato não encontrado."
    fi
}

# Função para excluir um contato existente
excluir_contato() {
    echo "Digite o nome do contato que deseja excluir:"
    read nome

    if grep -q "$nome" agenda.txt; then
        sed -i "/^$nome;/d" agenda.txt

        echo "Contato excluído com sucesso!"
    else
        echo "Contato não encontrado."
    fi
}

# Loop principal do programa
while true; do
    echo "-----------------------"
    echo "Agenda de contatos"
    echo "-----------------------"
    echo "Selecione uma opção:"
    echo "1 - Adicionar contato"
    echo "2 - Visualizar contatos"
    echo "3 - Editar contato"
    echo "4 - Excluir contato"
    echo "5 - Sair"
    echo "-----------------------"

    read opcao

    case $opcao in
        1) adicionar_contato;;
        2) visualizar_contatos;;
        3) editar_contato;;
        4) excluir_contato;;
        5) break;;
        *) echo "Opção inválida.";;
    esac
done
```

Neste código, utilizo algumas funções para realizar as operações básicas de uma agenda de contatos. 

- A função `adicionar_contato` pede ao usuário para digitar o nome, telefone e e-mail do contato e adiciona essas informações em um arquivo chamado "agenda.txt".

- A função `visualizar_contatos` lê o arquivo "agenda.txt" e exibe na tela todos os contatos cadastrados, mostrando o nome, telefone e e-mail de cada um.

- A função `editar_contato` pede ao usuário o nome do contato que deseja editar. Em seguida, permite que o usuário digite o novo telefone e novo e-mail do contato. O código utiliza o comando "sed" para substituir as informações antigas pelas novas no arquivo "agenda.txt".

- A função `excluir_contato` pede ao usuário o nome do contato que deseja excluir. Em seguida, utiliza o comando "sed" para excluir a linha correspondente ao contato no arquivo "agenda.txt".

- O loop principal do programa exibe um menu de opções para o usuário, onde ele pode escolher entre adicionar, visualizar, editar, excluir contatos ou sair do programa. O loop continua até que o usuário escolha a opção de sair.

Espero que este código atenda às suas expectativas!