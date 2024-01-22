```shell
#!/bin/bash

# Definir variables
directorio_actual=$(pwd)
lista_archivos=$(ls $directorio_actual)

# Empezar un bucle for para iterar sobre la lista de archivos
for archivo in $lista_archivos; do
  # Comprobar si el archivo es un directorio
  if [ -d $archivo ]; then
    # Si el archivo es un directorio, llamar a la función "procesar_directorio"
    procesar_directorio $archivo
  else
    # Si el archivo es un fichero, llamar a la función "procesar_fichero"
    procesar_fichero $archivo
  fi
done

# Definir la función "procesar_directorio"
procesar_directorio() {
  # Obtener el nombre del directorio
  directorio=$1

  # Cambiar al directorio especificado
  cd $directorio

  # Empezar un bucle for para iterar sobre la lista de archivos del directorio
  for archivo in $(ls); do
    # Comprobar si el archivo es un directorio
    if [ -d $archivo ]; then
      # Si el archivo es un directorio, llamar a la función "procesar_directorio"
      procesar_directorio $archivo
    else
      # Si el archivo es un fichero, llamar a la función "procesar_fichero"
      procesar_fichero $archivo
    fi
  done

  # Volver al directorio anterior
  cd ..
}

# Definir la función "procesar_fichero"
procesar_fichero() {
  # Obtener el nombre del fichero
  fichero=$1

  # Comprobar si el fichero es un fichero regular
  if [ -f $fichero ]; then
    # Si el fichero es un fichero regular, mostrar su nombre
    echo "Fichero: $fichero"
  fi
}
```

Explicación:

* El código empieza definiendo las variables `directorio_actual` y `lista_archivos`.
* Luego, empieza un bucle `for` para iterar sobre la lista de archivos del directorio actual.
* Dentro del bucle, se comprueba si el archivo es un directorio o un fichero.
* Si es un directorio, se llama a la función `procesar_directorio`, que procesa recursivamente todos los archivos del directorio.
* Si es un fichero, se llama a la función `procesar_fichero`, que muestra el nombre del fichero.
* La función `procesar_directorio` es similar a la función `procesar_fichero`, pero itera sobre la lista de archivos del directorio especificado.
* La función `procesar_fichero` simplemente muestra el nombre del fichero.