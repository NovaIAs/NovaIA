Aqui está um código complexo em TCL que realiza a verificação da validade de um CPF:

```tcl
# Função para verificar a validade de um CPF
proc verificarCPF {cpf} {
    # Remove os caracteres que não são dígitos
    set cpf [string map {"." "" "-" ""} $cpf]
    
    # Verifica se o CPF possui 11 dígitos
    if {[string length $cpf] != 11} {
        puts "CPF inválido. O CPF deve conter 11 dígitos."
        return
    }
    
    # Verifica se todos os dígitos são iguais
    if {[string is integer -strict $cpf] && [string match {*[string index $cpf 0]*} $cpf]} {
        puts "CPF inválido. Todos os dígitos são iguais."
        return
    }
    
    # Calcula o primeiro dígito verificador
    set soma 0
    for {set i 0} {$i < 9} {incr i} {
        set soma [expr {$soma + ([string index $cpf $i] * (10 - $i))}]
    }
    set resto [expr {$soma % 11}]
    if {$resto < 2} {
        set digito1 0
    } else {
        set digito1 [expr {11 - $resto}]
    }
    
    # Verifica se o primeiro dígito verificador está correto
    if {[string index $cpf 9] != $digito1} {
        puts "CPF inválido. Primeiro dígito verificador incorreto."
        return
    }
    
    # Calcula o segundo dígito verificador
    set soma 0
    for {set i 0} {$i < 10} {incr i} {
        set soma [expr {$soma + ([string index $cpf $i] * (11 - $i))}]
    }
    set resto [expr {$soma % 11}]
    if {$resto < 2} {
        set digito2 0
    } else {
        set digito2 [expr {11 - $resto}]
    }
    
    # Verifica se o segundo dígito verificador está correto
    if {[string index $cpf 10] != $digito2} {
        puts "CPF inválido. Segundo dígito verificador incorreto."
        return
    }
    
    puts "CPF válido."
}

# Exemplo de uso da função verificarCPF
verificarCPF "123.456.789-00"
```

Nesse código, a função `verificarCPF` recebe um CPF como parâmetro e realiza a verificação de sua validade. O código remove os caracteres especiais (pontos e traço), verifica se o CPF possui 11 dígitos e se todos os dígitos são iguais. Em seguida, calcula o primeiro dígito verificador e verifica se está correto. Em seguida, calcula o segundo dígito verificador e verifica se está correto. Caso alguma das verificações falhe, é exibida uma mensagem informando o motivo do CPF ser inválido. Caso contrário, é exibida uma mensagem informando que o CPF é válido.

No exemplo de uso, o CPF "123.456.789-00" é passado como parâmetro para a função `verificarCPF`.