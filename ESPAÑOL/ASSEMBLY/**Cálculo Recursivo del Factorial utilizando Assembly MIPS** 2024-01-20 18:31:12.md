```assembly (MIPS)
# Este programa calcula el factorial de un número utilizando recursion.

# Declarar variables
recursividad: .word 0  # Contador de recursividad
resultado: .word 1    # Resultado del factorial

# Función para calcular el factorial de un número
factorial:
    # Guardar el valor de recursividad actual en la pila
    sw $s0, 0($sp)
    
    # Guardar el valor del resultado actual en la pila
    sw $s1, 4($sp)
    
    # Incrementar el contador de recursividad
    addi $s0, $s0, 1
    
    # Comprobar si el número es 0 o 1
    beq $a0, 0, fin_recursividad
    beq $a0, 1, fin_recursividad
    
    # Calcular el factorial del número llamando a la función recursivamente
    subi $a0, $a0, 1  # Restar 1 al número
    jal factorial     # Llamar a la función factorial
    
    # Obtener el resultado del factorial de la llamada recursiva
    lw $t0, 4($sp)  # Cargar el resultado de la pila
    
    # Multiplicar el resultado actual por el resultado de la llamada recursiva
    mul $s1, $s1, $t0
    
# Fin de la función factorial
fin_recursividad:
    # Restaurar el valor de recursividad de la pila
    lw $s0, 0($sp)
    
    # Restaurar el valor del resultado de la pila
    lw $s1, 4($sp)
    
    # Regresar el resultado del factorial
    jr $ra

# Función principal
main:
    # Pedir al usuario que ingrese un número
    li $v0, 5
    syscall
    
    # Convertir el número ingresado a un entero
    li $t0, 10
    div $a0, $t0
    mfhi $a0
    
    # Llamar a la función factorial para calcular el factorial del número ingresado
    jal factorial
    
    # Imprimir el resultado del factorial
    li $v0, 1
    move $a0, $s1
    syscall
    
    # Finalizar el programa
    li $v0, 10
    syscall
```

Explicación:

* La función `factorial` calcula el factorial de un número utilizando recursión.
* La función `main` pide al usuario que ingrese un número, lo convierte a un entero y luego llama a la función `factorial` para calcular el factorial del número ingresado.
* El programa utiliza las siguientes variables:
    * `recursividad`: Contador de recursividad.
    * `resultado`: Resultado del factorial.
* El programa utiliza las siguientes funciones:
    * `factorial`: Calcula el factorial de un número utilizando recursión.
    * `main`: Función principal del programa.
* El programa utiliza los siguientes registros:
    * `$a0`: Número a calcular el factorial.
    * `$s0`: Contador de recursividad.
    * `$s1`: Resultado del factorial.
    * `$t0`: Variable temporal.
    * `$v0`: Registro de código de operación.