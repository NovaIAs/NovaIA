```shell
#!/bin/bash

# Variables
DIRECTORIO_ACTUAL=$(pwd)
FICHEROS_TEXTO=$(find $DIRECTORIO_ACTUAL -name '*.txt')
LINEAS_TOTALES=0
PALABRAS_TOTALES=0
CARACTERES_TOTALES=0

# Recorrer los ficheros de texto
for FICHERO in $FICHEROS_TEXTO; do
    # Contar las líneas del fichero
    LINEAS=$(wc -l < $FICHERO)
    LINEAS_TOTALES=$(( LINEAS_TOTALES + LINEAS ))

    # Contar las palabras del fichero
    PALABRAS=$(wc -w < $FICHERO)
    PALABRAS_TOTALES=$(( PALABRAS_TOTALES + PALABRAS ))

    # Contar los caracteres del fichero
    CARACTERES=$(wc -c < $FICHERO)
    CARACTERES_TOTALES=$(( CARACTERES_TOTALES + CARACTERES ))
done

# Calcular los promedios
PROMEDIO_LINEAS=$(( LINEAS_TOTALES / $(echo $FICHEROS_TEXTO | wc -w) ))
PROMEDIO_PALABRAS=$(( PALABRAS_TOTALES / $(echo $FICHEROS_TEXTO | wc -w) ))
PROMEDIO_CARACTERES=$(( CARACTERES_TOTALES / $(echo $FICHEROS_TEXTO | wc -w) ))

# Imprimir los resultados
echo "Número total de líneas: $LINEAS_TOTALES"
echo "Número total de palabras: $PALABRAS_TOTALES"
echo "Número total de caracteres: $CARACTERES_TOTALES"
echo "Promedio de líneas por fichero: $PROMEDIO_LINEAS"
echo "Promedio de palabras por fichero: $PROMEDIO_PALABRAS"
echo "Promedio de caracteres por fichero: $PROMEDIO_CARACTERES"
```

**Explicación del código:**

1. El script comienza definiendo algunas variables:

    * `DIRECTORIO_ACTUAL`: Almacena el directorio actual.
    * `FICHEROS_TEXTO`: Almacena una lista de todos los archivos de texto en el directorio actual.
    * `LINEAS_TOTALES`, `PALABRAS_TOTALES` y `CARACTERES_TOTALES`: Almacenan los totales de líneas, palabras y caracteres en todos los archivos de texto.

2. A continuación, el script entra en un bucle `for` que recorre todos los archivos de texto en `FICHEROS_TEXTO`.

3. Para cada archivo de texto, el script utiliza el comando `wc` para contar las líneas, palabras y caracteres en el archivo.

4. El script luego agrega estos totales a los totales acumulados en `LINEAS_TOTALES`, `PALABRAS_TOTALES` y `CARACTERES_TOTALES`.

5. Una vez que el script ha recorrido todos los archivos de texto, calcula los promedios de líneas, palabras y caracteres por archivo.

6. Finalmente, el script imprime los resultados en la consola.