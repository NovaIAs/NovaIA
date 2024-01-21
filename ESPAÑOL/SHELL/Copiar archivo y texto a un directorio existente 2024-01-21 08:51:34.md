```shell
#!/bin/bash

# 1. FUNCIONES

# Función para imprimir un mensaje de error y salir del programa
error() {
  echo "¡Error! $1" >&2
  exit 1
}

# Función para comprobar si un archivo existe
existe_archivo() {
  [ -f "$1" ]
}

# Función para comprobar si un directorio existe
existe_directorio() {
  [ -d "$1" ]
}

# 2. VARIABLES

# Variables para las rutas de los archivos y directorios
ruta_archivo_origen="./archivo_origen.txt"
ruta_archivo_destino="./archivo_destino.txt"
ruta_directorio_destino="./directorio_destino"

# Variable para el texto a copiar
texto="Este es el texto que se copiará"

# 3. COMPROBACIONES

# Comprobar si el archivo de origen existe
if ! existe_archivo "$ruta_archivo_origen"; then
  error "El archivo de origen no existe"
fi

# Comprobar si el directorio de destino existe
if ! existe_directorio "$ruta_directorio_destino"; then
  error "El directorio de destino no existe"
fi

# 4. COPIA DE ARCHIVOS

# Copiar el archivo de origen al archivo de destino
cp "$ruta_archivo_origen" "$ruta_archivo_destino"

# 5. COPIA DE TEXTO

# Abrir el archivo de destino en modo escritura
archivo_destino=$(open "$ruta_archivo_destino" w)

# Escribir el texto en el archivo de destino
echo "$texto" >> "$archivo_destino"

# Cerrar el archivo de destino
close "$archivo_destino"

# 6. MENSAJE DE ÉXITO

# Imprimir un mensaje de éxito
echo "¡Copia realizada con éxito!"

```

Explicación del código:

* Se definen tres funciones: `error()`, `existe_archivo()` y `existe_directorio()`. Estas funciones se utilizan para comprobar si hay errores, si un archivo existe y si un directorio existe, respectivamente.
* Se definen tres variables para las rutas de los archivos y directorios: `ruta_archivo_origen`, `ruta_archivo_destino` y `ruta_directorio_destino`.
* Se define una variable para el texto a copiar: `texto`.
* Se comprueban si el archivo de origen existe y si el directorio de destino existe. Si alguno de ellos no existe, se muestra un mensaje de error y se sale del programa.
* Se copia el archivo de origen al archivo de destino.
* Se abre el archivo de destino en modo escritura.
* Se escribe el texto en el archivo de destino.
* Se cierra el archivo de destino.
* Se imprime un mensaje de éxito.