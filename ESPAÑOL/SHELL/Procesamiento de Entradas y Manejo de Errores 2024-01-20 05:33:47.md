```shell
#!/bin/bash

# Definir las rutas de los archivos necesarios
archivo_entradas="./entradas.txt"
archivo_procesados="./procesados.txt"
archivo_errores="./errores.txt"

# Función para validar entradas
function validar_entradas() {
  local entrada="$1"

  # Comprobar si la entrada es un número
  if [[ ! "$entrada" =~ ^[0-9]+$ ]]; then
    return 1 # Retorna 1 si es una entrada no válida
  fi

  return 0 # Retorna 0 si es una entrada válida
}

# Función para procesar una entrada
function procesar_entrada() {
  local entrada="$1"

  # Calcular el cuadrado de la entrada
  cuadrado=$((entrada * entrada))

  # Escribir el resultado en el archivo de procesados
  echo "$cuadrado" >> "$archivo_procesados"
}

# Función para manejar los errores
function manejar_errores() {
  local error="$1"

  # Escribir el error en el archivo de errores
  echo "$error" >> "$archivo_errores"
}

# Leer las entradas línea por línea
while read -r linea; do
  # Validar la entrada
  if validar_entradas "$linea"; then
    # Procesar la entrada
    procesar_entrada "$linea"
  else
    # Manejar el error
    manejar_errores "Entrada no válida: $linea"
  fi
done < "$archivo_entradas"

# Mostrar el mensaje de finalización
echo "El procesamiento de las entradas ha finalizado."

```

Explicación del código:

1. **Definición de rutas de archivos:** Se definen las rutas de los archivos de entrada, procesamiento y errores.

2. **Función `validar_entradas`:** Esta función valida si una entrada es un número entero positivo. Si no lo es, devuelve 1; de lo contrario, devuelve 0.

3. **Función `procesar_entrada`:** Esta función calcula el cuadrado de una entrada dada.

4. **Función `manejar_errores`:** Esta función escribe un error en el archivo de errores.

5. **Procesamiento de las entradas:** Se lee cada línea del archivo de entradas y se valida. Si una entrada es válida, se procesa calculando su cuadrado y se escribe en el archivo de procesados. Si una entrada no es válida, se maneja el error escribiendo el error en el archivo de errores.

6. **Mensaje de finalización:** Una vez que todas las entradas se han procesado, se muestra un mensaje de finalización.