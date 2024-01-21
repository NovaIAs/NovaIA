```
#!/bin/bash

# Definir variables
color_rojo="\e[31m"
color_verde="\e[32m"
color_azul="\e[34m"
color_amarillo="\e[33m"
color_normal="\e[0m"

# Función para mostrar un mensaje en color
function mostrar_mensaje() {
  local mensaje="$1"
  local color="$2"
  echo -e "${color}${mensaje}${color_normal}"
}

# Función para leer una entrada del usuario
function leer_entrada() {
  local mensaje="$1"
  local variable="$2"
  echo -n "${mensaje}: "
  read "$variable"
}

# Función para validar una entrada numérica
function validar_numero() {
  local entrada="$1"
  local variable="$2"

  if [[ ! "$entrada" =~ ^[0-9]+$ ]]; then
    mostrar_mensaje "El valor ingresado no es un número válido" $color_rojo
    return 1
  fi

  eval "$variable=$entrada"
  return 0
}

# Función para validar una entrada de texto
function validar_texto() {
  local entrada="$1"
  local variable="$2"

  if [[ -z "$entrada" ]]; then
    mostrar_mensaje "El valor ingresado no puede estar vacío" $color_rojo
    return 1
  fi

  eval "$variable='$entrada'"
  return 0
}

# Función para calcular el área de un triángulo
function calcular_area_triangulo() {
  local base="$1"
  local altura="$2"
  local area=$(echo "scale=2; $base * $altura / 2" | bc)

  echo $area
}

# Función para calcular el área de un círculo
function calcular_area_circulo() {
  local radio="$1"
  local area=$(echo "scale=2; pi * $radio^2" | bc)

  echo $area
}

# Función para calcular el área de un cuadrado
function calcular_area_cuadrado() {
  local lado="$1"
  local area=$(echo "scale=2; $lado^2" | bc)

  echo $area
}

# Función para calcular el área de un rectángulo
function calcular_area_rectangulo() {
  local base="$1"
  local altura="$2"
  local area=$(echo "scale=2; $base * $altura" | bc)

  echo $area
}

# Función para mostrar el menú principal
function mostrar_menu_principal() {
  clear

  mostrar_mensaje "Menú principal" $color_azul
  echo "1. Calcular área de un triángulo"
  echo "2. Calcular área de un círculo"
  echo "3. Calcular área de un cuadrado"
  echo "4. Calcular área de un rectángulo"
  echo "5. Salir"

  leer_entrada "Ingrese una opción" opcion
}

# Función para mostrar el menú de opciones para el cálculo del área de un triángulo
function mostrar_menu_area_triangulo() {
  clear

  mostrar_mensaje "Cálculo del área de un triángulo" $color_azul
  echo "1. Ingresar base y altura"
  echo "2. Regresar al menú principal"

  leer_entrada "Ingrese una opción" opcion
}

# Función para mostrar el menú de opciones para el cálculo del área de un círculo
function mostrar_menu_area_circulo() {
  clear

  mostrar_mensaje "Cálculo del área de un círculo" $color_azul
  echo "1. Ingresar radio"
  echo "2. Regresar al menú principal"

  leer_entrada "Ingrese una opción" opcion
}

# Función para mostrar el menú de opciones para el cálculo del área de un cuadrado
function mostrar_menu_area_cuadrado() {
  clear

  mostrar_mensaje "Cálculo del área de un cuadrado" $color_azul
  echo "1. Ingresar lado"
  echo "2. Regresar al menú principal"

  leer_entrada "Ingrese una opción" opcion
}

# Función para mostrar el menú de opciones para el cálculo del área de un rectángulo
function mostrar_menu_area_rectangulo() {
  clear

  mostrar_mensaje "Cálculo del área de un rectángulo" $color_azul
  echo "1. Ingresar base y altura"
  echo "2. Regresar al menú principal"

  leer_entrada "Ingrese una opción" opcion
}

# Función para manejar la opción 1 del menú principal (calcular área de un triángulo)
function manejar_opcion_1() {
  while true; do
    mostrar_menu_area_triangulo

    case "$opcion" in
      1)
        leer_entrada "Ingrese la base del triángulo" base
        if ! validar_numero "$base"; then
          continue
        fi

        leer_entrada "Ingrese la altura del triángulo" altura
        if ! validar_numero "$altura"; then
          continue
        fi

        area=$(calcular_area_triangulo "$base" "$altura")
        mostrar_mensaje "El área del triángulo es: ${area} unidades cuadradas" $color_verde
        break
        ;;
      2)
        return
        ;;
      *)
        mostrar_mensaje "Opción no válida" $color_rojo
        continue
        ;;
    esac
  done
}

# Función para manejar la opción 2 del menú principal (calcular área de un círculo)
function manejar_opcion_2() {
  while true; do
    mostrar_menu_area_circulo

    case "$opcion" in
      1)
        leer_entrada "Ingrese el radio del círculo" radio
        if ! validar_numero "$radio"; then
          continue
        fi

        area=$(calcular_area_circulo "$radio")
        mostrar_mensaje "El área del círculo es: ${area} unidades cuadradas" $color_verde
        break
        ;;
      2)
        return
        ;;
      *)
        mostrar_mensaje "Opción no válida" $color_rojo
        continue
        ;;
    esac
  done
}

# Función para manejar la opción 3 del menú principal (calcular área de un cuadrado)
function manejar_opcion_3() {
  while true; do
    mostrar_menu_area_cuadrado

    case "$opcion" in
      1)
        leer_entrada "Ingrese el lado del cuadrado" lado
        if ! validar_numero "$lado"; then
          continue
        fi

        area=$(calcular_area_cuadrado "$lado")
        mostrar_mensaje "El área del cuadrado es: ${area} unidades cuadradas" $color_verde
        break
        ;;
      2)
        return
        ;;
      *)
        mostrar_mensaje "Opción no válida" $color_rojo
        continue
        ;;
    esac
  done
}

# Función para manejar la opción 4 del menú principal (calcular área de un rectángulo)
function manejar_opcion_4() {
  while true; do
    mostrar_menu_area_rectangulo

    case "$opcion" in
      1)
        leer_entrada "Ingrese la base del rectángulo" base
        if ! validar_numero "$base"; then
          continue
        fi

        leer_entrada "Ingrese la altura del rectángulo" altura
        if ! validar_numero "$altura"; then
          continue
        fi

        area=$(calcular_area_rectangulo "$base" "$altura")
        mostrar_mensaje "El área del rectángulo es: ${area} unidades cuadradas" $color_verde
        break
        ;;
      2)
        return
        ;;
      *)
        mostrar_mensaje "Opción no válida" $color_rojo
        continue
        ;;
    esac
  done
}

# Función para manejar la opción 5 del menú principal (salir)
function manejar_opcion_5() {
  clear
  mostrar_mensaje "Saliendo del programa" $color_azul
  exit 0
}

# Bucle principal del programa
while true; do
  mostrar_menu_principal

  case "$opcion" in
    1)
      manejar_opcion_1
      ;;
    2)
      manejar_opcion_2
      ;;
    3)
      manejar_opcion_3
      ;;
    4)
      manejar_opcion_4
      ;;
    5)
      manejar_opcion_5
      ;;
    *)
      mostrar_mensaje "Opción no válida" $color_rojo
      continue
      ;;
  esac
done
```

Este código es un programa que permite calcular el área de un triángulo, un círculo, un cuadrado y un rectángulo. El programa cuenta con un menú principal que permite al usuario seleccionar la figura geométrica para la que desea calcular el área. Cada figura geométrica tiene su propio menú secundario, donde el usuario puede ingresar los valores necesarios para el cálculo del área.

El programa utiliza funciones para validar la entrada del usuario, calcular el área de cada figura geométrica y mostrar los mensajes en color. También utiliza un bucle `while` para mantener el programa en ejecución y permitir al usuario calcular el área de diferentes figuras geométricas.

El código es complejo y diferenciado, ya que incluye múltiples funciones, bucles y condicionales para manejar las diferentes opciones del menú y los cálculos de las áreas. También utiliza colores para destacar los mensajes importantes y facilitar la lectura del programa.