```shell
#!/bin/bash

# Definimos una función para mostrar un menú
function mostrar_menu() {
    clear

    echo "Bienvenido al menú"
    echo "1. Opción 1"
    echo "2. Opción 2"
    echo "3. Opción 3"
    echo "4. Salir"

    read -p "Introduzca su opción: " opcion
}

# Definimos una función para ejecutar la opción seleccionada
function ejecutar_opcion() {
    case $opcion in
        1)
            echo "Has seleccionado la opción 1"
            ;;
        2)
            echo "Has seleccionado la opción 2"
            ;;
        3)
            echo "Has seleccionado la opción 3"
            ;;
        4)
            echo "Saliendo del programa"
            ;;
        *)
            echo "Opción no válida"
            ;;
    esac
}

# Definimos una función para repetir el menú hasta que el usuario seleccione la opción de salir
function ejecutar_menu() {
    while true; do
        mostrar_menu
        ejecutar_opcion

        if [ $opcion -eq 4 ]; then
            break
        fi
    done
}

# Llamamos a la función que ejecuta el menú
ejecutar_menu
```

Este código creará un menú interactivo en el terminal, con las siguientes opciones:

* Opción 1: Muestra un mensaje "Has seleccionado la opción 1".
* Opción 2: Muestra un mensaje "Has seleccionado la opción 2".
* Opción 3: Muestra un mensaje "Has seleccionado la opción 3".
* Opción 4: Sale del programa.

El usuario puede seleccionar una opción introduciendo un número del 1 al 4. Si introduce un número no válido, se le mostrará un mensaje de error.

El menú se repetirá hasta que el usuario seleccione la opción 4, que saldrá del programa.