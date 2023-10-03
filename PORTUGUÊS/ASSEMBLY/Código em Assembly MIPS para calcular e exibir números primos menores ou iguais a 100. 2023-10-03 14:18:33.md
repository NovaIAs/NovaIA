Claro! Aqui está um código complexo em Assembly que utiliza a linguagem de montagem MIPS para calcular e exibir todos os números primos menores ou iguais a 100:

```
# Definindo as constantes
.data
    newline:    .asciiz "\n"
    primo_msg:  .asciiz "Primo: "
    nao_primo_msg:  .asciiz "Nao primo: "
    
# Inicializando o registrador de contagem
    .text
    .globl main
main:
    li $t0, 2    # Inicializa o registrador de contagem com o valor 2
    
# Loop para verificar se os números são primos
loop:
    li $t1, 2    # Inicializa o registrador auxiliar com o valor 2
    move $t2, $t0   # Move o valor do registrador de contagem para o registrador auxiliar
    
    # Loop para verificar se o número é primo
inner_loop:
    beq $t2, $t1, print_prime    # Se os valores são iguais, o número é primo
    div $t2, $t1
    mfhi $t3    # Obter o resto da divisão
    
    beqz $t3, not_prime    # Se o resto for zero, o número não é primo
    
    addi $t1, $t1, 1    # Incrementa o valor do registrador auxiliar
    j inner_loop    # Volta ao início do loop interno
    
# Impressão do número primo
print_prime:
    li $v0, 4    # Carrega o código do syscall para imprimir string
    la $a0, primo_msg    # Carrega o endereço da mensagem "Primo: "
    syscall
    
    move $a0, $t2    # Move o número primo para o registrador de argumento
    
    li $v0, 1    # Carrega o código do syscall para imprimir inteiro
    syscall
    
    j next_number    # Pula para o próximo número
    
# Impressão do número não primo
not_prime:
    li $v0, 4    # Carrega o código do syscall para imprimir string
    la $a0, nao_primo_msg    # Carrega o endereço da mensagem "Nao primo: "
    syscall
    
    move $a0, $t2    # Move o número não primo para o registrador de argumento
    
    li $v0, 1    # Carrega o código do syscall para imprimir inteiro
    syscall
    
# Próximo número
next_number:
    addi $t0, $t0, 1    # Incrementa o valor do registrador de contagem
    li $t1, 2    # Reinicia o valor do registrador auxiliar
    
    ble $t0, 100, loop    # Se ainda não verificou todos os números, volta ao início do loop
    
    li $v0, 10    # Carrega o código do syscall para encerrar o programa
    syscall
```

Esse código em Assembly MIPS utiliza dois loops aninhados para verificar se um número é primo ou não. O loop externo começa com o registrador de contagem `$t0` sendo inicializado com o valor 2 e vai até 100. O loop interno é responsável por verificar se o número atual é divisível por algum número entre 2 e o número atual, `$t2`, utilizando o registrador auxiliar `$t1`. Se o número atual for divisível por algum número, ele não é primo e é exibida a mensagem "Nao primo: " seguida do número. Caso contrário, o número é primo e é exibida a mensagem "Primo: " seguida do número.

Espero que esse código complexo e diferenciado em Assembly atenda às suas expectativas!