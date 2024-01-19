```shell
#!/bin/bash

# Definición de variables
TITULO="Bienvenido al sistema de gestión de usuarios"
OPCIONES_MENU=(
    "Crear usuario"
    "Modificar usuario"
    "Eliminar usuario"
    "Listar usuarios"
    "Salir"
)

# Función para mostrar el menú
function mostrar_menu() {
    clear
    echo "$TITULO"
    echo "----------------------------------------"
    for (( i=0; i<${#OPCIONES_MENU[@]}; i++ )); do
        echo "$((i+1)). ${OPCIONES_MENU[i]}"
    done
    echo "----------------------------------------"
    echo "¿Qué desea hacer? (1-5):"
}

# Función para crear un usuario
function crear_usuario() {
    clear
    echo "Crear usuario"
    echo "----------------------------------------"

    # Solicitamos los datos del usuario
    echo "Nombre de usuario:"
    read NOMBRE_USUARIO

    echo "Contraseña:"
    read -s CONTRASEÑA

    echo "Nombre completo:"
    read NOMBRE_COMPLETO

    echo "Email:"
    read EMAIL

    # Creamos el usuario
    useradd -m -d /home/$NOMBRE_USUARIO -s /bin/bash $NOMBRE_USUARIO
    echo "$CONTRASEÑA" | passwd --stdin $NOMBRE_USUARIO

    # Actualizamos la información del usuario
    usermod -c "$NOMBRE_COMPLETO" $NOMBRE_USUARIO
    usermod -m -d /home/$NOMBRE_USUARIO $NOMBRE_USUARIO
    usermod -s /bin/bash $NOMBRE_USUARIO

    # Agregamos el usuario al grupo "usuarios"
    usermod -a -G usuarios $NOMBRE_USUARIO

    # Enviamos un correo electrónico al usuario
    echo "Hola $NOMBRE_COMPLETO,

    Se ha creado su cuenta de usuario en nuestro sistema.

    Su nombre de usuario es $NOMBRE_USUARIO
    Su contraseña es $CONTRASEÑA

    Atentamente,

    El administrador del sistema" | mail -s "Cuenta de usuario creada" $EMAIL

    # Volvemos al menú
    mostrar_menu
}

# Función para modificar un usuario
function modificar_usuario() {
    clear
    echo "Modificar usuario"
    echo "----------------------------------------"

    # Solicitamos el nombre del usuario a modificar
    echo "Nombre de usuario:"
    read NOMBRE_USUARIO

    # Verificamos si el usuario existe
    if id $NOMBRE_USUARIO >/dev/null 2>&1; then

        # Mostramos los datos actuales del usuario
        echo "Datos actuales del usuario:"
        echo "----------------------------------------"
        grep "^$NOMBRE_USUARIO:" /etc/passwd

        echo "¿Qué desea modificar?"
        echo "1. Nombre de usuario"
        echo "2. Contraseña"
        echo "3. Nombre completo"
        echo "4. Email"
        echo "5. Grupo"
        echo "6. Directorio personal"
        echo "7. Shell"
        echo "8. Volver al menú"
        echo "----------------------------------------"
        echo "Opción (1-8):"
        read OPCION

        case $OPCION in
            1)
                # Modificamos el nombre de usuario
                echo "Nuevo nombre de usuario:"
                read NUEVO_NOMBRE_USUARIO

                usermod -l $NUEVO_NOMBRE_USUARIO $NOMBRE_USUARIO
                ;;
            2)
                # Modificamos la contraseña
                echo "Nueva contraseña:"
                read -s NUEVA_CONTRASEÑA

                echo "$NUEVA_CONTRASEÑA" | passwd --stdin $NOMBRE_USUARIO
                ;;
            3)
                # Modificamos el nombre completo
                echo "Nuevo nombre completo:"
                read NUEVO_NOMBRE_COMPLETO

                usermod -c "$NUEVO_NOMBRE_COMPLETO" $NOMBRE_USUARIO
                ;;
            4)
                # Modificamos el email
                echo "Nuevo email:"
                read NUEVO_EMAIL

                usermod -m -d /home/$NOMBRE_USUARIO $NOMBRE_USUARIO
                ;;
            5)
                # Modificamos el grupo
                echo "Nuevo grupo:"
                read NUEVO_GRUPO

                usermod -a -G $NUEVO_GRUPO $NOMBRE_USUARIO
                ;;
            6)
                # Modificamos el directorio personal
                echo "Nuevo directorio personal:"
                read NUEVO_DIRECTORIO_PERSONAL

                usermod -d $NUEVO_DIRECTORIO_PERSONAL $NOMBRE_USUARIO
                ;;
            7)
                # Modificamos el shell
                echo "Nuevo shell:"
                read NUEVO_SHELL

                usermod -s $NUEVO_SHELL $NOMBRE_USUARIO
                ;;
            8)
                # Volvemos al menú
                mostrar_menu
                ;;
            *)
                echo "Opción inválida"
                ;;
        esac
    else
        echo "El usuario $NOMBRE_USUARIO no existe"
    fi

    # Volvemos al menú
    mostrar_menu
}

# Función para eliminar un usuario
function eliminar_usuario() {
    clear
    echo "Eliminar usuario"
    echo "----------------------------------------"

    # Solicitamos el nombre del usuario a eliminar
    echo "Nombre de usuario:"
    read NOMBRE_USUARIO

    # Verificamos si el usuario existe
    if id $NOMBRE_USUARIO >/dev/null 2>&1; then

        # Eliminamos el usuario
        userdel -r $NOMBRE_USUARIO

        echo "El usuario $NOMBRE_USUARIO ha sido eliminado"
    else
        echo "El usuario $NOMBRE_USUARIO no existe"
    fi

    # Volvemos al menú
    mostrar_menu
}

# Función para listar los usuarios
function listar_usuarios() {
    clear
    echo "Lista de usuarios"
    echo "----------------------------------------"

    # Listamos los usuarios
    cat /etc/passwd | grep "^[^:]*:" | cut -d ":" -f 1

    # Volvemos al menú
    mostrar_menu
}

# Función para salir del sistema
function salir() {
    exit 0
}

# Bucle principal del programa
while true; do
    mostrar_menu
    read OPCION

    case $OPCION in
        1)
            crear_usuario
            ;;
        2)
            modificar_usuario
            ;;
        3)
            eliminar_usuario
            ;;
        4)
            listar_usuarios
            ;;
        5)
            salir
            ;;
        *)
            echo "Opción inválida"
            ;;
    esac
done
```

Explicación del código:

- Se definen las variables necesarias para el funcionamiento del programa.
- Se define la función `mostrar_menu()`, que muestra el menú al usuario y le pide que seleccione una opción.
- Se definen las funciones `crear_usuario()`, `modificar_usuario()`, `eliminar_usuario()`, `listar_usuarios()` y `salir()`, que realizan las diferentes operaciones del programa.
- El programa entra en un bucle infinito, y en cada iteración se muestra el menú al usuario y se le pide que seleccione una opción.
- En función de la opción seleccionada, se llama a la función correspondiente para realizar la operación deseada.
- El programa finaliza cuando el usuario selecciona la opción "Salir".