Claro! Aqui está um código em TCL que cria uma calculadora simples em linha de comando:

```tcl
# Função para adição
proc adicao {num1 num2} {
    return [expr {$num1 + $num2}]
}

# Função para subtração
proc subtracao {num1 num2} {
    return [expr {$num1 - $num2}]
}

# Função para multiplicação
proc multiplicacao {num1 num2} {
    return [expr {$num1 * $num2}]
}

# Função para divisão
proc divisao {num1 num2} {
    if {$num2 != 0} {
        return [expr {$num1 / $num2}]
    } else {
        puts "Erro: Divisão por zero não é permitida!"
        return
    }
}

# Loop principal
while {1} {
    puts "Calculadora Simples"
    puts "Selecione uma operação:"
    puts "1. Adição"
    puts "2. Subtração"
    puts "3. Multiplicação"
    puts "4. Divisão"
    puts "5. Sair"
    puts "Digite o número correspondente à operação desejada: "
    set opcao [gets stdin]

    switch -exact $opcao {
        1 {
            puts "Digite o primeiro número: "
            set num1 [gets stdin]
            puts "Digite o segundo número: "
            set num2 [gets stdin]
            puts "Resultado: [adicao $num1 $num2]"
        }
        2 {
            puts "Digite o primeiro número: "
            set num1 [gets stdin]
            puts "Digite o segundo número: "
            set num2 [gets stdin]
            puts "Resultado: [subtracao $num1 $num2]"
        }
        3 {
            puts "Digite o primeiro número: "
            set num1 [gets stdin]
            puts "Digite o segundo número: "
            set num2 [gets stdin]
            puts "Resultado: [multiplicacao $num1 $num2]"
        }
        4 {
            puts "Digite o primeiro número: "
            set num1 [gets stdin]
            puts "Digite o segundo número: "
            set num2 [gets stdin]
            puts "Resultado: [divisao $num1 $num2]"
        }
        5 {
            puts "Saindo..."
            exit
        }
        default {
            puts "Opção inválida! Tente novamente."
        }
    }
}
```

Este código em TCL cria uma calculadora simples que permite ao usuário selecionar uma operação (adição, subtração, multiplicação ou divisão) e realizar cálculos com dois números fornecidos. 

As operações de adição, subtração, multiplicação e divisão são definidas como procedimentos (`proc`) que recebem dois números como argumentos e retornam o resultado da operação. A função `divisao` verifica se o segundo número é zero antes de realizar a divisão para evitar erros.

Em seguida, um loop principal (`while {1}`) é iniciado para permitir que o usuário faça várias operações sem sair do programa. Dentro do loop, o usuário é solicitado a selecionar uma operação digitando o número correspondente. Um comando `switch` é usado para executar o código correspondente a cada opção selecionada.

Se a opção selecionada for 1, 2, 3 ou 4, o usuário é solicitado a fornecer os números para a operação selecionada. Em seguida, o resultado é exibido usando a função apropriada. Se a opção selecionada for 5, o programa exibe uma mensagem de saída e sai do loop usando o comando `exit`. Se a opção selecionada for diferente de 1, 2, 3, 4 ou 5, uma mensagem de erro é exibida.

Espero que este código atenda às suas expectativas!