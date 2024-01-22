```shell
#!/bin/bash

# Se declara una variable para almacenar el directorio actual.
directorio_actual=$(pwd)

# Se declara una variable para almacenar el nombre del archivo temporal que se creará.
archivo_temporal=$(mktemp)

# Se declara una variable para almacenar el nombre del archivo de salida que se creará.
archivo_salida=${directorio_actual}/archivo_salida.txt

# Se declara una función para imprimir un mensaje de error y salir del script.
function error() {
  echo "Error: $1" >&2
  exit 1
}

# Se comprueba si el usuario ha proporcionado el nombre del archivo de entrada.
if [[ $# -lt 1 ]]; then
  error "Debes proporcionar el nombre del archivo de entrada."
fi

# Se almacena el nombre del archivo de entrada en una variable.
archivo_entrada=$1

# Se comprueba si el archivo de entrada existe y es un archivo regular.
if [[ ! -f "$archivo_entrada" ]]; then
  error "El archivo de entrada no existe o no es un archivo regular."
fi

# Se comprueba si el archivo de salida no existe o es un archivo regular.
if [[ -f "$archivo_salida" && ! -f "$archivo_salida" ]]; then
  error "El archivo de salida ya existe y no es un archivo regular."
fi

# Se abre el archivo de entrada en modo lectura.
exec 3<$archivo_entrada

# Se abre el archivo temporal en modo escritura.
exec 4>$archivo_temporal

# Se crea un bucle para leer las líneas del archivo de entrada.
while read -u 3 linea; do

  # Se elimina el espacio en blanco al principio y al final de la línea.
  linea=${linea//[[:space:]]/}

  # Se comprueba si la línea no está vacía.
  if [[ -n "$linea" ]]; then

    # Se añade la línea al archivo temporal.
    echo "$linea" >&4

  fi

done

# Se cierra el archivo de entrada.
exec 3<&-

# Se cierra el archivo temporal.
exec 4<&-

# Se ordenan las líneas del archivo temporal.
sort $archivo_temporal > $archivo_salida

# Se elimina el archivo temporal.
rm $archivo_temporal

# Se imprime un mensaje de éxito.
echo "El archivo de salida se ha creado correctamente."

```

Este código es un script que lee las líneas de un archivo de entrada, elimina el espacio en blanco al principio y al final de cada línea, y las añade a un archivo temporal. Luego, ordena las líneas del archivo temporal y las escribe en un archivo de salida. Finalmente, elimina el archivo temporal y muestra un mensaje de éxito.

El código utiliza la sintaxis de shell heredoc para definir la función `error()`. Esta sintaxis permite definir una función en varias líneas.

El código también utiliza la sintaxis de shell here string para definir el comando `sort`. Esta sintaxis permite especificar el comando a ejecutar en varias líneas.

El código utiliza la tubería de shell para concatenar los comandos `sort` y `rm`. Esta sintaxis permite ejecutar dos comandos en secuencia, y el resultado del primer comando se pasa como entrada al segundo comando.

El código utiliza el redireccionamiento de entrada y salida de shell para abrir y cerrar archivos. Esta sintaxis permite redirigir la entrada o salida estándar de un comando a un archivo o a otra entrada o salida estándar.

El código utiliza el comando `echo` para imprimir un mensaje de éxito. Este comando imprime el mensaje especificado en la salida estándar.