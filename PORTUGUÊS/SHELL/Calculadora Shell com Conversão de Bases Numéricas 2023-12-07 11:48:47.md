Claro! Segue um código em Shell que permite ao usuário realizar operações básicas de uma calculadora. Além disso, ele também permite a conversão entre bases numéricas decimal, binária e hexadecimal. Vou dividir o código em partes para melhor entendimento:

```bash
#!/bin/bash

# Função para exibir o menu principal
exibir_menu_principal() {
    clear
    echo "====== CALCULADORA ======"
    echo "1) Soma"
    echo "2) Subtração"
    echo "3) Multiplicação"
    echo "4) Divisão"
    echo "5) Conversão de bases numéricas"
    echo "0) Sair"
    echo ""
    read -p "Escolha uma opção: " opcao

    case $opcao in
        1)
            realizar_soma
            ;;
        2)
            realizar_subtracao
            ;;
        3)
            realizar_multiplicacao
            ;;
        4)
            realizar_divisao
            ;;
        5)
            realizar_conversao_bases
            ;;
        0)
            clear
            exit 0
            ;;
        *)
            echo "Opção inválida. Pressione ENTER para continuar..."
            read
            exibir_menu_principal
            ;;
    esac
}

# Função para realizar a soma
realizar_soma() {
    clear
    read -p "Digite o primeiro valor: " valor1
    read -p "Digite o segundo valor: " valor2

    resultado=$(($valor1 + $valor2))

    echo "Resultado: $valor1 + $valor2 = $resultado"
    echo "Pressione ENTER para continuar..."
    read
    exibir_menu_principal
}

# Função para realizar a subtração
realizar_subtracao() {
    clear
    read -p "Digite o primeiro valor: " valor1
    read -p "Digite o segundo valor: " valor2

    resultado=$(($valor1 - $valor2))

    echo "Resultado: $valor1 - $valor2 = $resultado"
    echo "Pressione ENTER para continuar..."
    read
    exibir_menu_principal
}

# Função para realizar a multiplicação
realizar_multiplicacao() {
    clear
    read -p "Digite o primeiro valor: " valor1
    read -p "Digite o segundo valor: " valor2

    resultado=$(($valor1 * $valor2))

    echo "Resultado: $valor1 x $valor2 = $resultado"
    echo "Pressione ENTER para continuar..."
    read
    exibir_menu_principal
}

# Função para realizar a divisão
realizar_divisao() {
    clear
    read -p "Digite o primeiro valor: " valor1
    read -p "Digite o segundo valor: " valor2

    resultado=$(($valor1 / $valor2))

    echo "Resultado: $valor1 ÷ $valor2 = $resultado"
    echo "Pressione ENTER para continuar..."
    read
    exibir_menu_principal
}

# Função para realizar a conversão de bases numéricas
realizar_conversao_bases() {
    clear
    echo "====== CONVERSÃO DE BASES NUMÉRICAS ======"
    echo "1) Decimal para Binário"
    echo "2) Decimal para Hexadecimal"
    echo "3) Binário para Decimal"
    echo "4) Binário para Hexadecimal"
    echo "5) Hexadecimal para Decimal"
    echo "6) Hexadecimal para Binário"
    echo "0) Voltar"
    echo ""
    read -p "Escolha uma opção: " opcao_conversao_bases

    case $opcao_conversao_bases in
        1)
            converter_decimal_para_binario
            ;;
        2)
            converter_decimal_para_hexadecimal
            ;;
        3)
            converter_binario_para_decimal
            ;;
        4)
            converter_binario_para_hexadecimal
            ;;
        5)
            converter_hexadecimal_para_decimal
            ;;
        6)
            converter_hexadecimal_para_binario
            ;;
        0)
            exibir_menu_principal
            ;;
        *)
            echo "Opção inválida. Pressione ENTER para continuar..."
            read
            realizar_conversao_bases
            ;;
    esac
}

# Função para converter um número decimal para binário
converter_decimal_para_binario() {
    clear
    read -p "Digite o número decimal: " decimal

    binario=$(echo "obase=2; $decimal" | bc)

    echo "Resultado: $decimal (decimal) = $binario (binário)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Função para converter um número decimal para hexadecimal
converter_decimal_para_hexadecimal() {
    clear
    read -p "Digite o número decimal: " decimal

    hexadecimal=$(printf "%X" $decimal)

    echo "Resultado: $decimal (decimal) = $hexadecimal (hexadecimal)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Função para converter um número binário para decimal
converter_binario_para_decimal() {
    clear
    read -p "Digite o número binário: " binario

    decimal=$(echo "ibase=2; $binario" | bc)

    echo "Resultado: $binario (binário) = $decimal (decimal)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Função para converter um número binário para hexadecimal
converter_binario_para_hexadecimal() {
    clear
    read -p "Digite o número binário: " binario

    decimal=$(echo "ibase=2; $binario" | bc)
    hexadecimal=$(printf "%X" $decimal)

    echo "Resultado: $binario (binário) = $hexadecimal (hexadecimal)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Função para converter um número hexadecimal para decimal
converter_hexadecimal_para_decimal() {
    clear
    read -p "Digite o número hexadecimal: " hexadecimal

    decimal=$(echo "ibase=16; $hexadecimal" | bc)

    echo "Resultado: $hexadecimal (hexadecimal) = $decimal (decimal)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Função para converter um número hexadecimal para binário
converter_hexadecimal_para_binario() {
    clear
    read -p "Digite o número hexadecimal: " hexadecimal

    decimal=$(echo "ibase=16; $hexadecimal" | bc)
    binario=$(echo "obase=2; $decimal" | bc)

    echo "Resultado: $hexadecimal (hexadecimal) = $binario (binário)"
    echo "Pressione ENTER para continuar..."
    read
    realizar_conversao_bases
}

# Chamada inicial para exibir o menu principal
exibir_menu_principal
```

Neste código, utilizamos funções para organizar a lógica das operações. O código começa exibindo um menu principal para que o usuário possa escolher uma opção. Após a escolha da opção, o código redireciona para a função correspondente.

As funções `realizar_soma()`, `realizar_subtracao()`, `realizar_multiplicacao()` e `realizar_divisao()` realizam as operações básicas da calculadora através da leitura de dois valores informados pelo usuário e exibem o resultado.

A função `realizar_conversao_bases()` exibe um submenu para que o usuário escolha o tipo de conversão de bases numéricas que deseja realizar. Essa função também redireciona para as funções específicas de cada tipo de conversão.

As funções de conversão, como `converter_decimal_para_binario()`, `converter_decimal_para_hexadecimal()` e outras, utilizam as ferramentas `bc` e `printf` para realizar as conversões entre as bases numéricas.

Espero que este código atenda às suas expectativas. Caso tenha alguma dúvida ou precise de mais explicações, fique à vontade para perguntar!