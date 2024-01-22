```shell
#!/bin/bash

# 1. Definir colores para el texto
ROJO="\033[1;31m"
VERDE="\033[1;32m"
AMARILLO="\033[1;33m"
AZUL="\033[1;34m"
MAGENTA="\033[1;35m"
CIAN="\033[1;36m"
SIN_COLOR="\033[0m"

# 2. Definir funciones

# Función para mostrar un mensaje de error
error() {
  echo -e "${ROJO}Error:${SIN_COLOR} $1" >&2
  exit 1
}

# Función para mostrar un mensaje de advertencia
advertencia() {
  echo -e "${AMARILLO}Advertencia:${SIN_COLOR} $1" >&2
}

# Función para mostrar un mensaje de información
info() {
  echo -e "${AZUL}Información:${SIN_COLOR} $1"
}

# Función para mostrar un mensaje de éxito
exito() {
  echo -e "${VERDE}Éxito:${SIN_COLOR} $1"
}

# 3. Obtener los argumentos del script

if [ $# -ne 1 ]; then
  error "Uso: $0 <nombre_archivo>"
fi

nombre_archivo="$1"

# 4. Comprobar si el archivo existe

if [ ! -f "$nombre_archivo" ]; then
  error "El archivo $nombre_archivo no existe"
fi

# 5. Leer el contenido del archivo

contenido=$(cat "$nombre_archivo")

# 6. Convertir el contenido del archivo a minúsculas

contenido=$(echo "$contenido" | tr '[:upper:]' '[:lower:]')

# 7. Contar el número de palabras en el archivo

num_palabras=$(echo "$contenido" | wc -w)

# 8. Contar el número de líneas en el archivo

num_lineas=$(echo "$contenido" | wc -l)

# 9. Contar el número de caracteres en el archivo

num_caracteres=$(echo "$contenido" | wc -c)

# 10. Mostrar los resultados

info "El archivo $nombre_archivo contiene:"
echo -e "${CIAN}Número de palabras:${SIN_COLOR} $num_palabras"
echo -e "${CIAN}Número de líneas:${SIN_COLOR} $num_lineas"
echo -e "${CIAN}Número de caracteres:${SIN_COLOR} $num_caracteres"

# 11. Comprobar si el archivo contiene una cadena de texto específica

cadena_de_texto="Hola"

if grep -qi "$cadena_de_texto" "$nombre_archivo"; then
  exito "El archivo $nombre_archivo contiene la cadena de texto $cadena_de_texto"
else
  advertencia "El archivo $nombre_archivo no contiene la cadena de texto $cadena_de_texto"
fi

# 12. Crear una copia de seguridad del archivo

nombre_copia_seguridad="$nombre_archivo.bak"

cp "$nombre_archivo" "$nombre_copia_seguridad"

exito "Se ha creado una copia de seguridad del archivo $nombre_archivo en $nombre_copia_seguridad"
```

Explicación del código:

* El código comienza definiendo una serie de colores que se utilizarán para mostrar mensajes en la consola.
* Luego define una serie de funciones para mostrar mensajes de error, advertencia, información y éxito.
* A continuación, el código obtiene los argumentos del script y comprueba si el archivo especificado en el primer argumento existe.
* Si el archivo existe, el código lee su contenido y lo convierte a minúsculas.
* Luego cuenta el número de palabras, líneas y caracteres en el archivo y muestra los resultados en la consola.
* El código también comprueba si el archivo contiene una cadena de texto específica y muestra un mensaje de éxito o advertencia en consecuencia.
* Por último, el código crea una copia de seguridad del archivo original con una extensión `.bak`.