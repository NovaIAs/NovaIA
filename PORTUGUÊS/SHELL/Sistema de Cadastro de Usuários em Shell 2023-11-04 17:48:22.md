Claro! Aqui está um código complexo em Shell, que cria um sistema de cadastro de usuários usando arquivos de texto como banco de dados:

```shell
#!/bin/bash

# Função para exibir o menu principal
exibir_menu() {
    clear
    echo "==== SISTEMA DE CADASTRO DE USUÁRIOS ===="
    echo
    echo "1) Cadastrar novo usuário"
    echo "2) Listar todos os usuários"
    echo "3) Buscar usuário por nome"
    echo "4) Remover usuário"
    echo "5) Sair"
    echo
    echo -n "Digite a opção desejada: "
}

# Função para cadastrar um novo usuário
cadastrar_usuario() {
    clear
    echo "==== CADASTRO DE NOVO USUÁRIO ===="
    echo
    echo -n "Digite o nome completo: "
    read nome
    echo -n "Digite o e-mail: "
    read email
    echo -n "Digite a idade: "
    read idade

    # Verifica se o arquivo de usuários já existe
    if [ -f usuarios.txt ]; then
        # Obtém o último ID cadastrado
        ultimo_id=$(tail -n 1 usuarios.txt | cut -d ';' -f 1)
        novo_id=$((ultimo_id + 1))
    else
        novo_id=1
    fi

    # Adiciona o novo usuário ao arquivo de usuários
    echo "$novo_id;$nome;$email;$idade" >> usuarios.txt

    echo
    echo "Usuário cadastrado com sucesso!"
    echo
    read -p "Pressione Enter para continuar..."
}

# Função para listar todos os usuários
listar_usuarios() {
    clear
    echo "==== LISTA DE USUÁRIOS ===="
    echo
    if [ -f usuarios.txt ]; then
        echo "ID | NOME                 | E-MAIL                | IDADE"
        echo "-----------------------------------------------------"
        cat usuarios.txt | while read linha; do
            id=$(echo $linha | cut -d ';' -f 1)
            nome=$(echo $linha | cut -d ';' -f 2)
            email=$(echo $linha | cut -d ';' -f 3)
            idade=$(echo $linha | cut -d ';' -f 4)
            printf "%-3s| %-20s| %-20s| %-4s\n" "$id" "$nome" "$email" "$idade"
        done
    else
        echo "Nenhum usuário cadastrado."
    fi
    echo
    read -p "Pressione Enter para continuar..."
}

# Função para buscar um usuário por nome
buscar_usuario() {
    clear
    echo "==== BUSCAR USUÁRIO POR NOME ===="
    echo
    echo -n "Digite o nome do usuário: "
    read nome_busca

    if [ -f usuarios.txt ]; then
        encontrado=0
        cat usuarios.txt | while read linha; do
            nome=$(echo $linha | cut -d ';' -f 2)
            if [ "$nome" = "$nome_busca" ]; then
                id=$(echo $linha | cut -d ';' -f 1)
                email=$(echo $linha | cut -d ';' -f 3)
                idade=$(echo $linha | cut -d ';' -f 4)

                echo
                echo "ID: $id"
                echo "Nome: $nome"
                echo "E-mail: $email"
                echo "Idade: $idade"

                encontrado=1
                break
            fi
        done

        if [ $encontrado -eq 0 ]; then
            echo
            echo "Nenhum usuário encontrado com o nome '$nome_busca'."
        fi
    else
        echo "Nenhum usuário cadastrado."
    fi
    echo
    read -p "Pressione Enter para continuar..."
}

# Função para remover um usuário
remover_usuario() {
    clear
    echo "==== REMOVER USUÁRIO ===="
    echo
    echo -n "Digite o ID do usuário que deseja remover: "
    read id_remover

    if [ -f usuarios.txt ]; then
        encontrado=0
        while read linha; do
            id=$(echo $linha | cut -d ';' -f 1)
            if [ "$id" = "$id_remover" ]; then
                encontrado=1
                break
            fi
        done < usuarios.txt

        if [ $encontrado -eq 1 ]; then
            sed -i "/^$id_remover;/d" usuarios.txt
            echo
            echo "Usuário removido com sucesso."
        else
            echo
            echo "Nenhum usuário encontrado com o ID '$id_remover'."
        fi
    else
        echo "Nenhum usuário cadastrado."
    fi
    echo
    read -p "Pressione Enter para continuar..."
}

# Loop principal do programa
while true; do
    exibir_menu
    read opcao

    case $opcao in
    1)
        cadastrar_usuario
        ;;
    2)
        listar_usuarios
        ;;
    3)
        buscar_usuario
        ;;
    4)
        remover_usuario
        ;;
    5)
        break
        ;;
    *)
        echo "Opção inválida. Por favor, tente novamente."
        echo
        read -p "Pressione Enter para continuar..."
        ;;
    esac
done
```

Este código cria um sistema de cadastro de usuários utilizando um arquivo de texto chamado `usuarios.txt` como banco de dados. O menu principal é exibido através da função `exibir_menu`, onde o usuário pode escolher entre cadastrar um novo usuário, listar todos os usuários, buscar um usuário por nome, remover um usuário ou sair do programa.

Cada opção do menu possui uma função correspondente: `cadastrar_usuario`, `listar_usuarios`, `buscar_usuario` e `remover_usuario`. Cada função realiza as ações necessárias, como ler os dados do usuário, adicionar ou remover registros do arquivo `usuarios.txt`, e exibir informações na tela. Após cada ação, o usuário é solicitado a pressionar Enter para continuar.

Espero que este código atenda às suas expectativas!