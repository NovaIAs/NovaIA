```shell
#!/bin/bash

# Definir variables
DIRECTORIO_ORIGEN="/ruta/a/directorio/origen"
DIRECTORIO_DESTINO="/ruta/a/directorio/destino"
ARCHIVO_CONFIGURACION="configuracion.txt"
COMANDO_COPIA="cp"

# Comprobar si el directorio de origen existe
if [ ! -d "$DIRECTORIO_ORIGEN" ]; then
  echo "El directorio de origen '$DIRECTORIO_ORIGEN' no existe."
  exit 1
fi

# Comprobar si el directorio de destino existe
if [ ! -d "$DIRECTORIO_DESTINO" ]; then
  echo "El directorio de destino '$DIRECTORIO_DESTINO' no existe."
  exit 1
fi

# Comprobar si el archivo de configuración existe
if [ ! -f "$ARCHIVO_CONFIGURACION" ]; then
  echo "El archivo de configuración '$ARCHIVO_CONFIGURACION' no existe."
  exit 1
fi

# Leer el archivo de configuración
while IFS=':' read -r CLAVE VALOR; do
  case "$CLAVE" in
    "modo_copia")
      COMANDO_COPIA="$VALOR"
      ;;
    *)
      echo "Clave desconocida '$CLAVE' en el archivo de configuración."
      exit 1
      ;;
  esac
done < "$ARCHIVO_CONFIGURACION"

# Copiar los archivos
echo "Copiando archivos de '$DIRECTORIO_ORIGEN' a '$DIRECTORIO_DESTINO'..."
"$COMANDO_COPIA" -r "$DIRECTORIO_ORIGEN" "$DIRECTORIO_DESTINO"

echo "Archivos copiados exitosamente."

```

Explicación del código:

- El código comienza definiendo las variables necesarias para la ejecución del script.

- Luego, se comprueba si el directorio de origen, el directorio de destino y el archivo de configuración existen. Si alguno de ellos no existe, se muestra un mensaje de error y se sale del script.

- A continuación, se lee el archivo de configuración. El archivo de configuración contiene pares clave-valor, separados por dos puntos (:). Cada par clave-valor se lee y se utiliza para establecer el valor de una variable.

- Una vez que se han leído todas las claves y valores del archivo de configuración, se utiliza el comando `cp` para copiar los archivos del directorio de origen al directorio de destino. El modo de copia se establece en función del valor de la clave `modo_copia` del archivo de configuración.

- Finalmente, se muestra un mensaje de confirmación indicando que los archivos se han copiado correctamente.