```shell
#!/bin/bash

# Variables globales
# --------------------
declare -r NOMBRE="Código Complejo en Shell"
declare -r VERSION=1.0

# Funciones
# ---------
funcion_saludar() {
    echo "Hola, soy $NOMBRE versión $VERSION"
}

funcion_despedirse() {
    echo "Adiós, hasta la próxima"
}

funcion_calcular_factorial() {
    local n=$1
    local resultado=1
    local i

    for (( i=2; i<=n; i++ )); do
        resultado=$(( resultado * i ))
    done

    echo $resultado
}

funcion_comprobar_primo() {
    local n=$1
    local es_primo=1    # 1 significa verdadero, 0 significa falso

    for (( i=2; i<n; i++ )); do
        if (( n%i == 0 )); then
            es_primo=0  # Si n es divisible entre algún número entre 2 y n-1, entonces no es primo
            break          # No es necesario seguir comprobando
        fi
    done

    echo $es_primo
}

# Programa principal
# -------------------
funcion_saludar

# Pedir un número al usuario
echo "Introduce un número:"
read numero

# Calcular el factorial del número
factorial=$(funcion_calcular_factorial $numero)
echo "El factorial de $numero es $factorial"

# Comprobar si el número es primo
es_primo=$(funcion_comprobar_primo $numero)
if (( es_primo == 1 )); then
    echo "$numero es primo"
else
    echo "$numero no es primo"
fi

funcion_despedirse
```

Explicación:

* El código comienza definiendo las variables globales `NOMBRE` y `VERSION`, que contienen el nombre y la versión del script, respectivamente.
* A continuación, se definen las funciones `funcion_saludar()`, `funcion_despedirse()`, `funcion_calcular_factorial()` y `funcion_comprobar_primo()`.
* La función `funcion_saludar()` simplemente saluda al usuario.
* La función `funcion_despedirse()` se utiliza para despedirse del usuario al final del script.
* La función `funcion_calcular_factorial()` calcula el factorial de un número dado.
* La función `funcion_comprobar_primo()` comprueba si un número dado es primo.
* El programa principal comienza llamando a la función `funcion_saludar()`.
* A continuación, se le pide al usuario que introduzca un número.
* El número introducido por el usuario se utiliza para calcular el factorial y para comprobar si es primo.
* Por último, se llama a la función `funcion_despedirse()` para despedirse del usuario.