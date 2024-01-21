```
#!/bin/bash

# Declaración de variables
nombre_fichero="fichero.txt"
texto="Este es un texto de ejemplo."
patron_busqueda="palabra"
reemplazo="nueva palabra"

# Comprobación de si el fichero existe
if [ ! -f $nombre_fichero ]; then
  echo "El fichero $nombre_fichero no existe."
  exit 1
fi

# Lectura del fichero
texto=$(cat $nombre_fichero)

# Búsqueda y reemplazo de texto
texto=$(echo $texto | sed "s/$patron_busqueda/$reemplazo/g")

# Escritura del fichero
echo $texto > $nombre_fichero

# Impresión del fichero
cat $nombre_fichero

```

Explicación:

1. Declaración de variables:

   * `nombre_fichero`: Nombre del fichero de texto que se va a procesar.
   * `texto`: Texto que se va a buscar y reemplazar.
   * `patron_busqueda`: Patrón de búsqueda que se va a utilizar.
   * `reemplazo`: Texto que se va a utilizar para reemplazar el patrón de búsqueda.

2. Comprobación de si el fichero existe:

   * Se utiliza el comando `if` para comprobar si el fichero `nombre_fichero` existe.
   * Si el fichero no existe, se imprime un mensaje de error y se sale del script.

3. Lectura del fichero:

   * Se utiliza el comando `cat` para leer el contenido del fichero `nombre_fichero` y almacenarlo en la variable `texto`.

4. Búsqueda y reemplazo de texto:

   * Se utiliza el comando `sed` para buscar y reemplazar el patrón de búsqueda `patron_busqueda` con el texto de reemplazo `reemplazo`.
   * Se utiliza la opción `-g` para que la búsqueda y el reemplazo se realicen en todas las líneas del texto.

5. Escritura del fichero:

   * Se utiliza el comando `echo` para escribir el texto modificado en el fichero `nombre_fichero`.

6. Impresión del fichero:

   * Se utiliza el comando `cat` para imprimir el contenido del fichero `nombre_fichero` en la consola.