```
#!/bin/bash

# Variables globales
function ayuda () {
  echo "Uso: ./script.sh [opciones] [argumentos]"
  echo "Opciones:"
  echo "  -h, --help      Mostrar esta ayuda y salir"
  echo "  -v, --verbose   Activar el modo verbosos"
  echo "  -f, --file      Especificar el archivo de entrada"
  echo "  -o, --output     Especificar el archivo de salida"
  echo "Argumentos:"
  echo "  archivo        Archivo de entrada"
  echo "  salida         Archivo de salida"
}

# Procesar opciones
while getopts "hvf:o:" opt; do
  case $opt in
    h) ayuda; exit 0;;
    v) verbose=true;;
    f) input_file=$OPTARG;;
    o) output_file=$OPTARG;;
    :) echo "Opción -$OPTARG requiere un argumento"; exit 1;;
    ?) echo "Opción -$OPTARG inválida"; exit 1;;
  esac
done

# Mostrar ayuda si no se especifican argumentos
if [[ $# -eq 0 ]]; then
  ayuda
  exit 0
fi

# Verificar que el archivo de entrada exista
if [[ ! -f $input_file ]]; then
  echo "El archivo de entrada $input_file no existe"
  exit 1
fi

# Crear el archivo de salida si no existe
if [[ ! -f $output_file ]]; then
  touch $output_file
fi

# Leer el archivo de entrada y procesar cada línea
while read line; do
  # Dividir la línea en campos
  IFS=',' read -a fields <<< "$line"

  # Procesar cada campo
  for field in "${fields[@]}"; do
    # Convertir el campo a mayúsculas
    field=$(echo $field | tr '[:lower:]' '[:upper:]')

    # Añadir el campo al archivo de salida
    echo $field >> $output_file
  done

  # Mostrar mensaje de progreso si el modo verbose está activado
  if [[ $verbose ]]; then
    echo "Procesando línea $line..."
  fi
done < $input_file

# Mostrar mensaje de finalización
echo "Proceso finalizado"
```

Explicación del código:

* El código comienza definiendo una función llamada `ayuda()` que muestra el uso del script y sale.
* A continuación, se procesan las opciones y argumentos especificados en la línea de comandos.
* Se verifica que el archivo de entrada exista y que el archivo de salida no exista.
* Se crea el archivo de salida si no existe.
* Se lee el archivo de entrada y se procesa cada línea.
* Cada línea se divide en campos utilizando el delimitador `,`.
* Se procesa cada campo convirtiéndolo a mayúsculas y añadiéndolo al archivo de salida.
* Si el modo verbose está activado, se muestra un mensaje de progreso.
* Por último, se muestra un mensaje de finalización.