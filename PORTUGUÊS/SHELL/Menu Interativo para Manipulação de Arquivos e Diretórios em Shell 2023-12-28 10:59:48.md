Claro! Aqui está um código complexo em Shell que realiza a manipulação de arquivos e diretorios. O código está comentado para facilitar o entendimento:

```shell
#!/bin/bash

# Função para exibir o menu principal
exibir_menu() {
    clear # Limpa a tela
    echo "=========== MENU PRINCIPAL ============"
    echo "1. Criar diretório"
    echo "2. Criar arquivo"
    echo "3. Listar diretório"
    echo "4. Copiar arquivo"
    echo "5. Mover arquivo"
    echo "6. Renomear arquivo"
    echo "7. Deletar arquivo/diretório"
    echo "8. Sair"
    echo "======================================="
}

# Função para criar um diretório
criar_diretorio() {
    echo "Digite o nome do diretório que deseja criar:"
    read nome_diretorio
    mkdir $nome_diretorio
    echo "Diretório $nome_diretorio criado com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Função para criar um arquivo
criar_arquivo() {
    echo "Digite o nome do arquivo que deseja criar:"
    read nome_arquivo
    echo "Digite o conteúdo do arquivo (pressione Ctrl + D para finalizar):"
    cat > $nome_arquivo
    echo "Arquivo $nome_arquivo criado com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Função para listar o conteúdo de um diretório
listar_diretorio() {
    echo "Digite o caminho do diretório que deseja listar (ou '.' para o diretório atual):"
    read caminho_diretorio
    ls -l $caminho_diretorio
    read -p "Pressione ENTER para continuar..."
}

# Função para copiar um arquivo
copiar_arquivo() {
    echo "Digite o caminho do arquivo que deseja copiar:"
    read caminho_arquivo_origem
    echo "Digite o caminho de destino para a cópia:"
    read caminho_arquivo_destino
    cp $caminho_arquivo_origem $caminho_arquivo_destino
    echo "Arquivo copiado com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Função para mover um arquivo
mover_arquivo() {
    echo "Digite o caminho do arquivo que deseja mover:"
    read caminho_arquivo_origem
    echo "Digite o caminho de destino para a movimentação:"
    read caminho_arquivo_destino
    mv $caminho_arquivo_origem $caminho_arquivo_destino
    echo "Arquivo movido com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Função para renomear um arquivo
renomear_arquivo() {
    echo "Digite o caminho do arquivo que deseja renomear:"
    read caminho_arquivo_origem
    echo "Digite o novo nome para o arquivo:"
    read novo_nome_arquivo
    mv $caminho_arquivo_origem $novo_nome_arquivo
    echo "Arquivo renomeado com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Função para deletar um arquivo ou diretório
deletar_arquivo_diretorio() {
    echo "Digite o caminho do arquivo/diretório que deseja deletar:"
    read caminho_arquivo_diretorio
    rm -rf $caminho_arquivo_diretorio
    echo "Arquivo/diretório deletado com sucesso!"
    read -p "Pressione ENTER para continuar..."
}

# Loop principal do programa
while true; do
    exibir_menu # Exibe o menu principal
    echo "Digite a opção desejada:"
    read opcao

    case $opcao in
        1) criar_diretorio ;;
        2) criar_arquivo ;;
        3) listar_diretorio ;;
        4) copiar_arquivo ;;
        5) mover_arquivo ;;
        6) renomear_arquivo ;;
        7) deletar_arquivo_diretorio ;;
        8) break ;;
        *) echo "Opção inválida!"; read -p "Pressione ENTER para continuar..."; ;;
    esac
done

echo "Programa finalizado!"
```

Este código em Shell é um menu interativo que permite ao usuário realizar diversas operações em arquivos e diretórios. Ele contém funções para criar diretórios, criar arquivos, listar conteúdos de diretórios, copiar arquivos, mover arquivos, renomear arquivos e deletar arquivos/diretórios. O programa mantém um loop principal que exibe um menu ao usuário, aguarda a entrada de uma opção e executa a função correspondente à opção escolhida. O programa só será finalizado quando a opção "Sair" for selecionada.