```shell
#!/bin/bash

# Definir variables
nombre="Juan"
apellido="García"
edad=25
casado=false
hijos=0

# Función para saludar
function saludar() {
  echo "Hola, mi nombre es $nombre $apellido. Tengo $edad años"
  if [ $casado = true ]; then
    echo "Estoy casado"
  else
    echo "Estoy soltero"
  fi
  if [ $hijos -gt 0 ]; then
    echo "Tengo $hijos hijos"
  else
    echo "No tengo hijos"
  fi
}

# Función para calcular el área de un triángulo
function calcular_area_triangulo() {
  base=$1
  altura=$2
  area=$(echo "scale=2; $base * $altura / 2" | bc)
  echo $area
}

# Función para imprimir un menú
function imprimir_menu() {
  echo "1. Saludar"
  echo "2. Calcular área de un triángulo"
  echo "3. Salir"
}

# Bucle principal del programa
while true; do
  # Imprimir el menú
  imprimir_menu

  # Leer la opción del usuario
  read opcion

  # Ejecutar la opción seleccionada
  case $opcion in
    1)
      saludar
      ;;
    2)
      echo "Ingrese la base del triángulo:"
      read base
      echo "Ingrese la altura del triángulo:"
      read altura
      area=$(calcular_area_triangulo $base $altura)
      echo "El área del triángulo es: $area"
      ;;
    3)
      echo "Saliendo del programa..."
      exit 0
      ;;
    *)
      echo "Opción no válida"
      ;;
  esac
done
```

Este código es un programa interactivo que permite al usuario saludar, calcular el área de un triángulo o salir del programa.

El código comienza definiendo algunas variables, incluyendo el nombre, apellido, edad, estado civil y número de hijos del usuario.

Luego, define tres funciones:

* `saludar()`: Esta función saluda al usuario por su nombre, edad y estado civil.
* `calcular_area_triangulo()`: Esta función calcula el área de un triángulo dado su base y altura.
* `imprimir_menu()`: Esta función imprime un menú de opciones para el usuario.

El código principal del programa es un bucle `while` que se ejecuta hasta que el usuario selecciona la opción de salir. En cada iteración del bucle, se imprime el menú y se lee la opción del usuario. Luego, se ejecuta la función correspondiente a la opción seleccionada.

Si el usuario selecciona la opción de saludar, se llama a la función `saludar()`. Si el usuario selecciona la opción de calcular el área de un triángulo, se llama a la función `calcular_area_triangulo()`. Si el usuario selecciona la opción de salir, se llama a la función `exit()` para salir del programa.