Claro! Aqui está um exemplo de um código complexo em Shell que realiza uma série de operações matemáticas em um arquivo de dados:

```shell
#!/bin/bash

# Função para calcular a média de um conjunto de números
calcular_media() {
    local soma=0
    local contador=0

    while read -r numero; do
        soma=$((soma + numero))
        contador=$((contador + 1))
    done < "$1"

    media=$(bc <<< "scale=2; $soma / $contador")
    echo "A média é: $media"
}

# Função para calcular o desvio padrão de um conjunto de números
calcular_desvio_padrao() {
    local soma=0
    local contador=0
    local media
    local desvio_padrao

    while read -r numero; do
        soma=$((soma + numero))
        contador=$((contador + 1))
    done < "$1"

    media=$(bc <<< "scale=2; $soma / $contador")

    soma_diferenca_quadrado=0
    while read -r numero; do
        diferenca=$((numero - media))
        soma_diferenca_quadrado=$(bc <<< "scale=2; $soma_diferenca_quadrado + ($diferenca * $diferenca)")
    done < "$1"

    variancia=$(bc <<< "scale=2; $soma_diferenca_quadrado / $contador")
    desvio_padrao=$(bc <<< "scale=2; sqrt($variancia)")

    echo "O desvio padrão é: $desvio_padrao"
}

# Função para calcular o máximo e mínimo de um conjunto de números
calcular_maximo_minimo() {
    local maximo=-999999999
    local minimo=999999999

    while read -r numero; do
        if [[ $numero -gt $maximo ]]; then
            maximo=$numero
        fi

        if [[ $numero -lt $minimo ]]; then
            minimo=$numero
        fi
    done < "$1"

    echo "O máximo é: $maximo"
    echo "O mínimo é: $minimo"
}

# Menu principal
menu_principal() {
    clear
    echo "===== Análise Estatística ====="
    echo "1. Calcular média"
    echo "2. Calcular desvio padrão"
    echo "3. Calcular máximo e mínimo"
    echo "0. Sair"
    echo "==============================="
    echo "Selecione uma opção: "
}

# Loop principal
opcao=-1
while [[ $opcao != 0 ]]; do
    menu_principal
    read -r opcao

    case $opcao in
        1)
            clear
            echo "===== Calcular Média ====="
            echo "Digite o caminho para o arquivo de dados:"
            read -r arquivo
            if [[ -f $arquivo ]]; then
                calcular_media "$arquivo"
            else
                echo "Arquivo não encontrado!"
            fi
            read -n 1 -s -r -p "Pressione qualquer tecla para continuar..."
            ;;
        2)
            clear
            echo "===== Calcular Desvio Padrão ====="
            echo "Digite o caminho para o arquivo de dados:"
            read -r arquivo
            if [[ -f $arquivo ]]; then
                calcular_desvio_padrao "$arquivo"
            else
                echo "Arquivo não encontrado!"
            fi
            read -n 1 -s -r -p "Pressione qualquer tecla para continuar..."
            ;;
        3)
            clear
            echo "===== Calcular Máximo e Mínimo ====="
            echo "Digite o caminho para o arquivo de dados:"
            read -r arquivo
            if [[ -f $arquivo ]]; then
                calcular_maximo_minimo "$arquivo"
            else
                echo "Arquivo não encontrado!"
            fi
            read -n 1 -s -r -p "Pressione qualquer tecla para continuar..."
            ;;
        0)
            clear
            echo "Encerrando o programa..."
            sleep 2
            ;;
        *)
            clear
            echo "Opção inválida! Tente novamente."
            read -n 1 -s -r -p "Pressione qualquer tecla para continuar..."
            ;;
    esac
done
```

Neste código, temos uma série de funções para calcular a média, desvio padrão, máximo e mínimo de um conjunto de números lidos a partir de um arquivo de dados. O programa exibe um menu principal onde o usuário pode selecionar a operação desejada. Em seguida, o programa solicita o caminho para o arquivo de dados e realiza o cálculo correspondente. Após cada operação, o programa aguarda a entrada do usuário antes de retornar ao menu principal. O programa encerra quando o usuário seleciona a opção de sair (0).