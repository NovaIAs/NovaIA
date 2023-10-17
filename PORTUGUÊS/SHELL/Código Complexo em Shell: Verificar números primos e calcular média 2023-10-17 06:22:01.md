Claro, vou gerar um código complexo em Shell para você. Aqui está:

```shell
#!/bin/bash

# Função para verificar se um número é primo
function verificar_primo() {
    num=$1
    divisor=2
    while [[ divisor -le num/2 ]]; do
        if [[ num%divisor -eq 0 ]]; then
            return 1
        fi
        divisor=$((divisor+1))
    done
    return 0
}

# Função para calcular a média de uma lista de números
function calcular_media() {
    soma=0
    qtd_numeros=$1
    shift
    for numero in "$@"; do
        soma=$((soma+numero))
    done
    media=$((soma/qtd_numeros))
    echo "A média é: $media"
}

# Função principal
function main() {
    echo "Bem-vindo ao programa complexo em Shell!"
    read -p "Digite um número: " num
    verificar_primo "$num"
    if [[ $? -eq 0 ]]; then
        echo "$num é primo."
    else
        echo "$num não é primo."
    fi

    read -p "Digite a quantidade de números para calcular a média: " qtd_numeros
    read -p "Digite os números separados por espaço: " numeros
    calcular_media "$qtd_numeros" $numeros
}

# Chamar a função principal
main
```

Explicação do código:

- O código começa com a declaração do shebang `#!/bin/bash`, que indica que o interpretador do shell a ser utilizado é o bash.

- Em seguida, temos a definição da função `verificar_primo()`, que recebe um número como parâmetro e verifica se ele é primo ou não. A função utiliza um loop while para iterar de 2 até a metade do número fornecido, verificando se ele é divisível por algum outro número. Se for divisível, a função retorna 1; caso contrário, retorna 0.

- A função `calcular_media()` é definida para calcular a média de uma lista de números. Ela recebe como primeiro parâmetro a quantidade de números a serem considerados e os demais parâmetros são os próprios números. A função utiliza um loop for para iterar sobre os números e calcular a soma. Em seguida, a média é calculada dividindo a soma pela quantidade de números e é exibida na tela.

- A função principal `main()` é definida para interagir com o usuário. Ela exibe uma mensagem de boas-vindas, solicita um número para verificar se é primo, chama a função `verificar_primo()` e exibe o resultado.

- Em seguida, o programa solicita a quantidade de números para calcular a média e os números em si. Esses valores são passados como parâmetros para a função `calcular_media()`, que exibe o resultado na tela.

- Por fim, a função principal `main()` é chamada para iniciar a execução do programa.

Espero que este código complexo em Shell atenda às suas expectativas!