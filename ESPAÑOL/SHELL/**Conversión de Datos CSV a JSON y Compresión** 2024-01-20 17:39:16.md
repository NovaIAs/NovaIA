```shell
#!/bin/bash

# Definir variables globales
RUTA_DE_DATOS="/ruta/a/datos/crudos"
RUTA_DE_SALIDA="/ruta/a/datos/procesados"
FORMATO_DE_ENTRADA="csv"
FORMATO_DE_SALIDA="json"

# Función para verificar si un directorio existe
function verificar_directorio() {
  if [ ! -d "$1" ]; then
    mkdir -p "$1"
  fi
}

# Función para convertir archivos CSV a JSON
function convertir_csv_a_json() {
  awk -F, '{printf "{\"%s\": \"%s\",", $1, $2}' "$1" \
      | sed 's/.$//; print "}"' \
      > "$2"
}

# Crear directorios necesarios
verificar_directorio "$RUTA_DE_SALIDA"

# Convertir todos los archivos CSV en el directorio de datos crudos a JSON
for archivo in "$RUTA_DE_DATOS"/*.csv; do
  nombre_de_archivo=$(basename "$archivo" | sed 's/.csv//')
  ruta_de_salida="$RUTA_DE_SALIDA/$nombre_de_archivo.$FORMATO_DE_SALIDA"
  convertir_csv_a_json "$archivo" "$ruta_de_salida"
done

# Comprimir archivos JSON en el directorio de datos procesados
for archivo in "$RUTA_DE_SALIDA"/*.json; do
  gzip "$archivo"
done

# Imprimir un mensaje de finalización
echo "Los datos se han procesado exitosamente."
```

Explicación del código:

1. Definir variables globales:
   - `RUTA_DE_DATOS`: ruta al directorio que contiene los datos crudos en formato CSV.
   - `RUTA_DE_SALIDA`: ruta al directorio donde se guardarán los datos procesados en formato JSON.
   - `FORMATO_DE_ENTRADA`: formato de los datos de entrada (CSV).
   - `FORMATO_DE_SALIDA`: formato de los datos de salida (JSON).

2. Función `verificar_directorio`:
   - Esta función verifica si un directorio existe y, si no existe, lo crea.
   - Se utiliza para crear los directorios necesarios para almacenar los datos procesados.

3. Función `convertir_csv_a_json`:
   - Esta función convierte un archivo CSV en un archivo JSON.
   - Utiliza el comando `awk` para extraer los datos del archivo CSV y formatearlos en JSON.
   - El comando `sed` se utiliza para eliminar el último carácter de cada línea del archivo JSON (una coma).

4. Crear directorios necesarios:
   - Se llama a la función `verificar_directorio` para crear el directorio de datos procesados.

5. Convertir todos los archivos CSV en el directorio de datos crudos a JSON:
   - Se utiliza un bucle `for` para iterar sobre todos los archivos CSV en el directorio de datos crudos.
   - Para cada archivo CSV, se llama a la función `convertir_csv_a_json` para convertirlo a JSON.
   - El archivo JSON se guarda en el directorio de datos procesados con el mismo nombre que el archivo CSV original, pero con la extensión `.json`.

6. Comprimir archivos JSON en el directorio de datos procesados:
   - Se utiliza un bucle `for` para iterar sobre todos los archivos JSON en el directorio de datos procesados.
   - Para cada archivo JSON, se utiliza el comando `gzip` para comprimirlo.

7. Imprimir un mensaje de finalización:
   - Se imprime un mensaje en la consola para indicar que el procesamiento de datos se ha completado con éxito.