```shell
#!/bin/bash

# Declaración de variables.
fichero_entrada="fichero_entrada.txt"
fichero_salida="fichero_salida.txt"
cadena_buscar="cadena_a_buscar"
cadena_reemplazar="cadena_a_reemplazar"
patron_regex="^cadena_a_buscar$"

# Comprobar si el fichero de entrada existe.
if [ ! -f "$fichero_entrada" ]; then
  echo "Error: El fichero $fichero_entrada no existe."
  exit 1
fi

# Abrir el fichero de entrada y el fichero de salida.
fichero_entrada_descriptor=$(exec 3< "$fichero_entrada")
fichero_salida_descriptor=$(exec 4> "$fichero_salida")

# Leer el fichero de entrada línea por línea.
while read -u 3 linea; do
  # Comprobar si la línea contiene la cadena a buscar.
  if [[ "$linea" =~ $patron_regex ]]; then
    # Reemplazar la cadena a buscar por la cadena a reemplazar.
    linea=${linea//$cadena_buscar/$cadena_reemplazar}
  fi

  # Escribir la línea en el fichero de salida.
  echo "$linea" >&4
done

# Cerrar los ficheros de entrada y salida.
exec 3>&-
exec 4>&-

```

**Explicación del código:**

* El código comienza declarando las variables que se utilizarán en el script.
* A continuación, se comprueba si el fichero de entrada existe. Si no existe, se muestra un mensaje de error y se sale del script.
* Se abren el fichero de entrada y el fichero de salida con los descriptores de fichero 3 y 4, respectivamente.
* Se lee el fichero de entrada línea por línea.
* Para cada línea, se comprueba si contiene la cadena a buscar. Si la contiene, se reemplaza la cadena a buscar por la cadena a reemplazar.
* La línea modificada se escribe en el fichero de salida.
* Una vez que se han procesado todas las líneas del fichero de entrada, se cierran el fichero de entrada y el fichero de salida.