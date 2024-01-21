```shell
#!/bin/bash

# Definir variables
ruta_directorio=/home/usuario/directorio
archivo_texto=archivo.txt
patron_busqueda="cadena_a_buscar"
extension_archivo=".mp3"

# Función para buscar archivos en un directorio
function buscar_archivos() {
  ruta_directorio=$1
  patron_busqueda=$2
  extension_archivo=$3

  # Obtener una lista de todos los archivos en el directorio
  archivos=$(find "$ruta_directorio" -name "*$extension_archivo" -print)

  # Iterar sobre la lista de archivos
  for archivo in $archivos; do
    # Comprobar si el archivo contiene el patrón de búsqueda
    if grep -q "$patron_busqueda" "$archivo"; then
      # Mostrar el nombre del archivo
      echo "Archivo encontrado: $archivo"
    fi
  done
}

# Función para crear un archivo de registro
function crear_archivo_registro() {
  archivo_texto=$1

  # Crear un archivo nuevo o sobrescribir el existente
  > "$archivo_texto"

  # Añadir una línea al archivo
  echo "Línea añadida al archivo de registro" >> "$archivo_texto"
}

# Función para enviar un correo electrónico
function enviar_correo_electronico() {
  destinatario=$1
  asunto=$2
  mensaje=$3

  # Enviar un correo electrónico utilizando el comando "mail"
  mail -s "$asunto" "$destinatario" <<< "$mensaje"
}

# Función principal
function main() {
  # Buscar archivos en el directorio especificado
  buscar_archivos "$ruta_directorio" "$patron_busqueda" "$extension_archivo"

  # Crear un archivo de registro
  crear_archivo_registro "$archivo_texto"

  # Enviar un correo electrónico al administrador
  enviar_correo_electronico "administrador@dominio.com" "Informe diario" "El informe diario ha sido generado y se adjunta el archivo de registro."
}

# Ejecutar la función principal
main
```

Explicación del código:

* La primera parte del código define variables que se utilizarán en el script.
* Se definen tres funciones:
    * `buscar_archivos()`: Esta función busca archivos en un directorio especificado que coincidan con un patrón de búsqueda y una extensión de archivo especificada.
    * `crear_archivo_registro()`: Esta función crea un nuevo archivo de texto o sobrescribe el existente y añade una línea al archivo.
    * `enviar_correo_electronico()`: Esta función envía un correo electrónico a un destinatario especificado con un asunto y un mensaje especificado.
* La función `main()` se define como la función principal del script.
* La función `main()` llama a las funciones `buscar_archivos()`, `crear_archivo_registro()` y `enviar_correo_electronico()` para realizar las siguientes tareas:
    * Buscar archivos en el directorio especificado que coincidan con el patrón de búsqueda y la extensión de archivo especificada.
    * Crear un archivo de registro con una línea añadida.
    * Enviar un correo electrónico al administrador con el informe diario y el archivo de registro adjunto.
* La última línea del código ejecuta la función `main()` para iniciar el script.