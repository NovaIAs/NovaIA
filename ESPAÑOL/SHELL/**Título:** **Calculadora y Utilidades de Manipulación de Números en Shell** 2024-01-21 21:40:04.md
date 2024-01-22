```shell
#!/bin/bash

# Definición de variables
MENSAJE="Hola, Mundo!"
NUMERO=10
FECHA=$(date +"%d/%m/%Y")
HORA=$(date +"%H:%M:%S")

# Función para imprimir un mensaje
function imprimir_mensaje() {
    echo "$1"
}

# Función para sumar dos números
function sumar_numeros() {
    local SUMA=$(($1 + $2))
    echo "$SUMA"
}

# Función para restar dos números
function restar_numeros() {
    local RESTA=$(($1 - $2))
    echo "$RESTA"
}

# Función para multiplicar dos números
function multiplicar_numeros() {
    local PRODUCTO=$(($1 * $2))
    echo "$PRODUCTO"
}

# Función para dividir dos números
function dividir_numeros() {
    local COCIENTE=$(($1 / $2))
    echo "$COCIENTE"
}

# Función para calcular el factorial de un número
function factorial_numero() {
    local FACTORIAL=1
    for ((i = 1; i <= $1; i++)); do
        FACTORIAL=$(($FACTORIAL * $i))
    done
    echo "$FACTORIAL"
}

# Función para comprobar si un número es primo
function es_primo() {
    local ES_PRIMO=1
    for ((i = 2; i <= $(($1 / 2)); i++)); do
        if [ $(($1 % $i)) -eq 0 ]; then
            ES_PRIMO=0
            break
        fi
    done
    echo "$ES_PRIMO"
}

# Función para imprimir el menú
function imprimir_menu() {
    clear
    echo "Menú principal"
    echo "==================="
    echo "1. Imprimir un mensaje"
    echo "2. Sumar dos números"
    echo "3. Restar dos números"
    echo "4. Multiplicar dos números"
    echo "5. Dividir dos números"
    echo "6. Calcular el factorial de un número"
    echo "7. Comprobar si un número es primo"
    echo "8. Salir"
    echo "==================="
    echo -n "Elige una opción: "
}

# Función para leer una opción del usuario
function leer_opcion() {
    local OPCION
    read OPCION
    case $OPCION in
        1)
            imprimir_mensaje "$MENSAJE"
            ;;
        2)
            local NUM1
            local NUM2
            echo -n "Introduce el primer número: "
            read NUM1
            echo -n "Introduce el segundo número: "
            read NUM2
            local RESULTADO=$(sumar_numeros $NUM1 $NUM2)
            echo "El resultado de la suma es $RESULTADO"
            ;;
        3)
            local NUM1
            local NUM2
            echo -n "Introduce el primer número: "
            read NUM1
            echo -n "Introduce el segundo número: "
            read NUM2
            local RESULTADO=$(restar_numeros $NUM1 $NUM2)
            echo "El resultado de la resta es $RESULTADO"
            ;;
        4)
            local NUM1
            local NUM2
            echo -n "Introduce el primer número: "
            read NUM1
            echo -n "Introduce el segundo número: "
            read NUM2
            local RESULTADO=$(multiplicar_numeros $NUM1 $NUM2)
            echo "El resultado de la multiplicación es $RESULTADO"
            ;;
        5)
            local NUM1
            local NUM2
            echo -n "Introduce el primer número: "
            read NUM1
            echo -n "Introduce el segundo número: "
            read NUM2
            local RESULTADO=$(dividir_numeros $NUM1 $NUM2)
            echo "El resultado de la división es $RESULTADO"
            ;;
        6)
            local NUM
            echo -n "Introduce un número: "
            read NUM
            local RESULTADO=$(factorial_numero $NUM)
            echo "El factorial del número $NUM es $RESULTADO"
            ;;
        7)
            local NUM
            echo -n "Introduce un número: "
            read NUM
            local RESULTADO=$(es_primo $NUM)
            if [ $RESULTADO -eq 1 ]; then
                echo "El número $NUM es primo"
            else
                echo "El número $NUM no es primo"
            fi
            ;;
        8)
            exit 0
            ;;
        *)
            echo "Opción no válida"
            ;;
    esac
}

# Bucle principal del programa
while true; do
    imprimir_menu
    leer_opcion
done
```

Explicación del código:

* Se definen varias variables, incluyendo el mensaje a imprimir, un número, la fecha y la hora.
* Se definen varias funciones, incluyendo una para imprimir un mensaje, otra para sumar dos números, otra para restar dos números, otra para multiplicar dos números, otra para dividir dos números, otra para calcular el factorial de un número, y otra para comprobar si un número es primo.
* Se define una función para imprimir el menú principal del programa.
* Se define una función para leer la opción elegida por el usuario.
* El bucle principal del programa muestra el menú y lee la opción elegida por el usuario. En función de esta opción, el programa llama a la función correspondiente.