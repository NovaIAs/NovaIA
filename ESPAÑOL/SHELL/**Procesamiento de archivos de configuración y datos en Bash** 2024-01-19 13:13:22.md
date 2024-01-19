```shell
#!/bin/bash

# Definición de variables

# Ruta del directorio donde se encuentran los archivos de configuración
CONFIG_DIR="/etc/my-app"

# Nombre del archivo de configuración a utilizar
CONFIG_FILE="config.ini"

# Ruta del directorio donde se encuentran los datos de la aplicación
DATA_DIR="/var/lib/my-app"

# Nombre del archivo de datos a utilizar
DATA_FILE="data.txt"

# Leer el archivo de configuración y obtener los valores de las propiedades
CONFIG=$(grep -v '^;' "$CONFIG_DIR/$CONFIG_FILE" | grep -v '^$')

# Procesar los valores de las propiedades
for PROP in $CONFIG; do
  KEY=$(echo "$PROP" | cut -d= -f1)
  VAL=$(echo "$PROP" | cut -d= -f2)
  export "$KEY"="$VAL"
done

# Procesar los datos del archivo de datos
DATA=$(awk -F, '{print $1, $2, $3}' "$DATA_DIR/$DATA_FILE")

# Procesamiento adicional personalizado

# Mostrar los resultados del procesamiento
echo "Resultado: $RESULTADO"

```

Explicación del código:

1. **Definición de variables:** Se definen las rutas de los directorios y archivos de configuración y datos, así como el nombre del archivo de configuración y el archivo de datos a utilizar.

2. **Lectura del archivo de configuración:** Se lee el archivo de configuración y se extraen los valores de las propiedades, ignorando las líneas en blanco y las que empiezan por un punto y coma (;).

3. **Procesamiento de los valores de las propiedades:** Se recorren los valores de las propiedades y se exportan como variables de entorno, utilizando la clave de la propiedad como nombre de variable y el valor de la propiedad como valor de la variable.

4. **Procesamiento de los datos del archivo de datos:** Se lee el archivo de datos y se extraen los datos, separando cada campo por una coma (,).

5. **Procesamiento adicional personalizado:** Aquí se puede añadir cualquier procesamiento adicional que se necesite, como cálculos, operaciones de filtrado, etc.

6. **Mostrar los resultados del procesamiento:** Se muestra el resultado del procesamiento, que puede ser un mensaje, un valor o cualquier otra cosa que se haya generado en el procesamiento adicional personalizado.