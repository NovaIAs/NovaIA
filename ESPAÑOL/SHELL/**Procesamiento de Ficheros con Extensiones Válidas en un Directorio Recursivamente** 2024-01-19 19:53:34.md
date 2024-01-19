```shell
#!/bin/bash

# Definir variables
DIRECTORIO_RAIZ="/home/usuario/Documentos"
EXTENSIONES_VALIDAS="(.txt|.pdf|.docx|.xlsx)"
FICHEROS_ENCONTRADOS=()
FICHEROS_PROCESADOS=0
FICHEROS_NO_PROCESADOS=0

# Función para buscar ficheros recursivamente en un directorio
function buscar_ficheros() {
  local directorio=$1
  local extensiones=$2

  # Obtener todos los ficheros en el directorio actual
  local ficheros=$(find "$directorio" -type f -maxdepth 1)

  # Iterar sobre los ficheros encontrados
  for fichero in $ficheros; do
    # Comprobar si el fichero tiene una extensión válida
    if [[ $fichero =~ $extensiones ]]; then
      # Añadir el fichero a la lista de ficheros encontrados
      FICHEROS_ENCONTRADOS+=("$fichero")
    fi
  done

  # Obtener todos los subdirectorios en el directorio actual
  local subdirectorios=$(find "$directorio" -type d -maxdepth 1)

  # Iterar sobre los subdirectorios encontrados
  for subdirectorio in $subdirectorios; do
    # Buscar ficheros recursivamente en el subdirectorio
    buscar_ficheros "$subdirectorio" "$extensiones"
  done
}

# Función para procesar un fichero
function procesar_fichero() {
  local fichero=$1

  # Realizar alguna operación de procesamiento en el fichero
  # Por ejemplo, moverlo a otro directorio, renombrarlo o extraer información de él
  echo "Procesando fichero: $fichero"

  # Incrementar el contador de ficheros procesados
  FICHEROS_PROCESADOS=$((FICHEROS_PROCESADOS+1))
}

# Función para mostrar estadísticas
function mostrar_estadisticas() {
  echo "Total de ficheros encontrados: ${#FICHEROS_ENCONTRADOS[@]}"
  echo "Total de ficheros procesados: $FICHEROS_PROCESADOS"
  echo "Total de ficheros no procesados: $FICHEROS_NO_PROCESADOS"
}

# Llamar a la función para buscar ficheros
buscar_ficheros "$DIRECTORIO_RAIZ" "$EXTENSIONES_VALIDAS"

# Iterar sobre los ficheros encontrados
for fichero in ${FICHEROS_ENCONTRADOS[@]}; do
  # Procesar el fichero
  procesar_fichero "$fichero"
done

# Mostrar estadísticas
mostrar_estadisticas
```

Explicación:

* El código comienza definiendo algunas variables, incluyendo el directorio raíz en el que se buscarán los ficheros, las extensiones de fichero válidas y listas vacías para almacenar los ficheros encontrados, los ficheros procesados y los ficheros no procesados.
* La función `buscar_ficheros()` se define para buscar ficheros recursivamente en un directorio. Recibe dos parámetros: el directorio en el que se quiere buscar y las extensiones válidas de los ficheros. La función utiliza el comando `find` para buscar todos los ficheros en el directorio actual y sus subdirectorios, y los añade a la lista de ficheros encontrados si tienen una extensión válida.
* La función `procesar_fichero()` se define para procesar un fichero. Recibe un parámetro: el fichero que se quiere procesar. La función realiza alguna operación de procesamiento en el fichero, como moverlo a otro directorio, renombrarlo o extraer información de él.
* La función `mostrar_estadisticas()` se define para mostrar estadísticas sobre el número de ficheros encontrados, procesados y no procesados.
* El código llama a la función `buscar_ficheros()` para buscar ficheros en el directorio raíz especificado.
* El código itera sobre los ficheros encontrados y llama a la función `procesar_fichero()` para procesar cada fichero.
* El código llama a la función `mostrar_estadisticas()` para mostrar estadísticas sobre el número de ficheros encontrados, procesados y no procesados.