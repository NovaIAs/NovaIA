Claro! Aqui está um exemplo de código complexo em Shell que envolve manipulação de arquivos e operações matemáticas:

```shell
#!/bin/bash

# Função para calcular o fatorial de um número
fatorial() {
    if [ $1 -eq 0 ] || [ $1 -eq 1 ]; then
        echo 1
    else
        echo $(( $1 * $(fatorial $(( $1 - 1 ))) ))
    fi
}

# Função para verificar se um número é primo
eh_primo() {
    local num=$1
    local i=2

    while [ $i -le $(( $num / 2 )) ]; do
        if [ $(( $num % $i )) -eq 0 ]; then
            return 1
        fi
        i=$(( $i + 1 ))
    done

    return 0
}

# Função para criar um arquivo com números primos e seus fatoriais
criar_arquivo_primos_fatoriais() {
    local arquivo=$1
    local limite=$2

    echo "Calculando números primos e seus fatoriais..."

    rm -f $arquivo
    touch $arquivo

    for (( num=2; num<=$limite; num++ )); do
        if eh_primo $num; then
            fat=$(fatorial $num)
            echo "$num: $fat" >> $arquivo
        fi
    done

    echo "Cálculo concluído e arquivo $arquivo criado com sucesso!"
}

# Solicita ao usuário o limite máximo para cálculo
read -p "Digite o limite máximo para cálculo: " limite_maximo

# Chama a função para criar o arquivo de primos e fatoriais
criar_arquivo_primos_fatoriais "primos_fatoriais.txt" $limite_maximo
```

Este código em Shell apresenta um exemplo de utilização de funções para calcular o fatorial de um número e verificar se um número é primo. Em seguida, é criado um arquivo chamado "primos_fatoriais.txt" contendo os números primos e seus respectivos fatoriais, até um limite máximo informado pelo usuário.

O código começa definindo a função `fatorial`, que utiliza recursão para calcular o fatorial de um número. Em seguida, a função `eh_primo` é definida para verificar se um número é primo ou não, utilizando um loop while.

A função `criar_arquivo_primos_fatoriais` é responsável por criar o arquivo de saída contendo os números primos e seus fatoriais. Ela utiliza as funções `eh_primo` e `fatorial` para calcular e armazenar os valores.

No final do código, o usuário é solicitado a digitar o limite máximo para o cálculo. Em seguida, a função `criar_arquivo_primos_fatoriais` é chamada, passando o nome do arquivo de saída e o limite máximo informado pelo usuário como argumentos.

Espero que isso atenda às suas expectativas! Lembre-se de que você pode personalizar e adaptar este código conforme necessário.