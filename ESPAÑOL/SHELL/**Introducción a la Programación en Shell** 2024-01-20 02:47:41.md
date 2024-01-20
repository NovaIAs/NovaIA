```shell
#!/bin/bash

# Definir funciones
function saludo() {
    echo "Hola, soy una función"
}

function despedida() {
    echo "Adiós, me voy"
}

# Definir variables
nombre="Juan"
edad=20

# Bucle while
while [ $edad -lt 30 ]
do
    echo "Tengo $edad años"
    edad=$(($edad + 1))
done

# Bucle for
for i in 1 2 3 4 5
do
    echo "El valor de i es $i"
done

# Condicional if
if [ $edad -ge 30 ]
then
    echo "Soy mayor de 30 años"
elif [ $edad -ge 20 ]
then
    echo "Soy mayor de 20 años"
else
    echo "Soy menor de 20 años"
fi

# Condicional case
case $nombre in
    "Juan")
        echo "Hola, Juan"
        ;;
    "María")
        echo "Hola, María"
        ;;
    *)
        echo "Hola, desconocido"
        ;;
esac

# Llamar a funciones
saludo
despedida

```

Explicación del código:

* Definición de funciones: Se definen dos funciones, `saludo()` y `despedida()`, que imprimen mensajes en la pantalla.
* Definición de variables: Se definen dos variables, `nombre` y `edad`, que se utilizan en el código.
* Bucle `while`: Se utiliza un bucle `while` para imprimir los valores de la variable `edad` mientras sea menor que 30.
* Bucle `for`: Se utiliza un bucle `for` para imprimir los valores de la variable `i` de 1 a 5.
* Condicional `if`: Se utiliza un condicional `if` para imprimir un mensaje diferente en función del valor de la variable `edad`.
* Condicional `case`: Se utiliza un condicional `case` para imprimir un mensaje diferente en función del valor de la variable `nombre`.
* Llamada a funciones: Se llaman a las funciones `saludo()` y `despedida()` para imprimir mensajes en la pantalla.