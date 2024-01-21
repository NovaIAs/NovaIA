```assembly (MIPS)
# Definición de las constantes
base: .asciiz "Introduce la base: "
exp: .asciiz "Introduce el exponente: "
resultado: .asciiz "El resultado es: "

# Función exponencial
potencia_entera(base, exponente) {
    if (exponente == 0) {
        return 1;
    } else if (exponente < 0) {
        return 1 / potencia_entera(base, -exponente);
    } else {
        return base * potencia_entera(base, exponente - 1);
    }
}

# Función principal
main:
    # Imprimir el mensaje de introducción
    li $v0, 4
    la $a0, base
    syscall

    # Leer la base del teclado
    li $v0, 5
    syscall
    move $t0, $v0

    # Imprimir el mensaje de introducción
    li $v0, 4
    la $a0, exp
    syscall

    # Leer el exponente del teclado
    li $v0, 5
    syscall
    move $t1, $v0

    # Llamar a la función exponencial
    jal potencia_entera
    move $t2, $v0

    # Imprimir el resultado
    li $v0, 4
    la $a0, resultado
    syscall

    move $a0, $t2
    li $v0, 1
    syscall

    # Retornar 0 para indicar que el programa ha finalizado correctamente
    li $v0, 0
    syscall
}
```

Explicación:

* La función `potencia_entera` calcula la potencia de una base elevada a un exponente entero. La función utiliza la recursividad para calcular el resultado.
* La función `main` es la función principal del programa. Esta función se encarga de imprimir los mensajes de introducción, leer la base y el exponente del teclado, llamar a la función `potencia_entera` para calcular el resultado y finalmente imprimir el resultado.
* El programa utiliza las siguientes llamadas al sistema:
    * `li`: cargar un valor inmediato en un registro.
    * `syscall`: llamar al sistema operativo.
    * `la`: cargar la dirección de una variable en un registro.
    * `move`: mover el contenido de un registro a otro.
    * `jal`: saltar a una dirección.
    * `jr`: regresar de una llamada a una función.