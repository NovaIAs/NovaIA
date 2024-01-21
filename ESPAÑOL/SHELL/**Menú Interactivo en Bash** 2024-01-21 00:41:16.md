```shell
#!/bin/bash

# Variables globales
MENSAJE_BIENVENIDA="Bienvenido al programa"
MENSAJE_DESPEDIDA="Gracias por utilizar el programa"
OPCIONES_MENU="1) Opción 1\n2) Opción 2\n3) Opción 3\n4) Salir"

# Función para mostrar el menú
mostrar_menu() {
  echo "$MENSAJE_BIENVENIDA"
  echo "$OPCIONES_MENU"
}

# Función para obtener la opción seleccionada por el usuario
obtener_opcion() {
  read -p "Seleccione una opción: " OPCION
}

# Función para ejecutar la opción seleccionada por el usuario
ejecutar_opcion() {
  case $OPCION in
    1)
      echo "Ha seleccionado la opción 1"
      ;;
    2)
      echo "Ha seleccionado la opción 2"
      ;;
    3)
      echo "Ha seleccionado la opción 3"
      ;;
    4)
      echo "$MENSAJE_DESPEDIDA"
      exit 0
      ;;
    *)
      echo "Opción inválida"
      ;;
  esac
}

# Bucle principal del programa
while true; do
  mostrar_menu
  obtener_opcion
  ejecutar_opcion
done
```

Explicación del código:

* La primera línea del código indica al sistema operativo que se trata de un script de shell.
* A continuación, se declaran las variables globales que se utilizarán en el script.
* La función `mostrar_menu()` muestra el menú al usuario.
* La función `obtener_opcion()` obtiene la opción seleccionada por el usuario.
* La función `ejecutar_opcion()` ejecuta la opción seleccionada por el usuario.
* El bucle `while true` se utiliza para repetir el menú hasta que el usuario seleccione la opción de salir.