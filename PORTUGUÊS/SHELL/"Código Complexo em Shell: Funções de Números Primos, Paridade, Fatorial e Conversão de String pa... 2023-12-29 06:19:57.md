Claro! Aqui está um código complexo em Shell que utiliza um conjunto diversificado de recursos e comandos:

```shell
#!/bin/bash

# Função para verificar se um número é primo
function ehPrimo() {
    local num=$1
    local i
    local metade=$((num/2))

    if [ $num -eq 1 ]; then
        return 1
    fi

    for ((i=2; i <= metade; i++)); do
        if [ $((num%i)) -eq 0 ]; then
            return 1
        fi
    done

    return 0
}

# Função para verificar se um número é par ou ímpar
function ehPar() {
    local num=$1

    if [ $((num%2)) -eq 0 ]; then
        return 0
    else
        return 1
    fi
}

# Função para calcular o fatorial de um número
function fatorial() {
    local num=$1
    local resultado=1

    for ((i=2; i <= num; i++)); do
        resultado=$((resultado * i))
    done

    echo $resultado
}

# Função para converter uma string para maiúscula
function paraMaiuscula() {
    local str=$1

    echo $str | tr '[:lower:]' '[:upper:]'
}

# Função principal
function main() {
    echo "Bem-vindo ao programa complexo em Shell!"
    echo

    # Ler input do usuário
    read -p "Digite um número inteiro: " numero

    echo

    # Verificar se o número é primo
    if ehPrimo $numero; then
        echo "$numero é um número primo."
    else
        echo "$numero não é um número primo."
    fi

    echo

    # Verificar se o número é par ou ímpar
    if ehPar $numero; then
        echo "$numero é um número par."
    else
        echo "$numero é um número ímpar."
    fi

    echo

    # Calcular o fatorial do número
    fatorial=$(fatorial $numero)
    echo "O fatorial de $numero é $fatorial."

    echo

    # Converter uma string para maiúscula
    read -p "Digite uma string: " string
    string_maiuscula=$(paraMaiuscula "$string")
    echo "A string em maiúscula é: $string_maiuscula"

    echo

    echo "Programa complexo em Shell finalizado."
}

# Executar função principal
main
```

Neste código, temos várias funções que realizam diferentes tarefas:

1. A função `ehPrimo()` recebe um número como parâmetro e verifica se ele é primo.
2. A função `ehPar()` recebe um número como parâmetro e verifica se ele é par ou ímpar.
3. A função `fatorial()` recebe um número como parâmetro e calcula o seu fatorial.
4. A função `paraMaiuscula()` recebe uma string como parâmetro e a converte para maiúscula.
5. A função `main()` é a função principal do programa e interage com o usuário, utilizando as funções anteriores para realizar diferentes operações.

No programa principal, o usuário é solicitado a inserir um número inteiro. Em seguida, o programa verifica se o número é primo e exibe a resposta. Em seguida, o programa verifica se o número é par ou ímpar e exibe a resposta. Em seguida, o programa calcula o fatorial do número e o exibe. Por fim, o usuário é solicitado a inserir uma string, que é convertida para maiúscula e exibida na tela.

Espero que este código complexo em Shell atenda às suas expectativas!