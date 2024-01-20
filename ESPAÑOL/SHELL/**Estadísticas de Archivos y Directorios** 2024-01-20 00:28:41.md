```
#!/bin/bash

# Declaración de variables
NUM_ARCHIVOS=0
NUM_DIRECTORIOS=0
TAM_TOTAL=0
ARCHIVOS_VACIOS=""
ARCH_MAS_GRANDE=""
TAM_ARCH_MAS_GRANDE=0
DIR_MAS_PEQUEÑO=""
TAM_DIR_MAS_PEQUEÑO=999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999

# Recorrido recursivo de directorios
function recorrerDirectorios() {
    for elemento in "$1"/*; do
        if [ -d "$elemento" ]; then
            ((NUM_DIRECTORIOS++))
            recorrerDirectorios "$elemento"
        elif [ -f "$elemento" ]; then
            ((NUM_ARCHIVOS++))
            TAM=$(stat -c%s "$elemento")
            ((TAM_TOTAL+=$TAM))
            if [ $TAM -eq 0 ]; then
                ARCHIVOS_VACIOS="$ARCHIVOS_VACIOS\n$elemento"
            elif [ $TAM -gt $TAM_ARCH_MAS_GRANDE ]; then
                ARCH_MAS_GRANDE="$elemento"
                TAM_ARCH_MAS_GRANDE=$TAM
            fi
        fi
    done
}

# Obtención del directorio actual
DIR_ACTUAL=$(pwd)

# Recorrido recursivo del directorio actual
recorrerDirectorios "$DIR_ACTUAL"

# Cálculo del tamaño promedio de los archivos
TAM_PROM=$((TAM_TOTAL / NUM_ARCHIVOS))

# Obtención del directorio más pequeño
for dir in "$DIR_ACTUAL"/*; do
    if [ -d "$dir" ]; then
        TAM=$(du -s "$dir" | cut -f1)
        if [ $TAM -lt $TAM_DIR_MAS_PEQUEÑO ]; then
            DIR_MAS_PEQUEÑO="$dir"
            TAM_DIR_MAS_PEQUEÑO=$TAM
        fi
    fi
done

# Impresión de los resultados
echo "Número de archivos: $NUM_ARCHIVOS"
echo "Número de directorios: $NUM_DIRECTORIOS"
echo "Tamaño total de los archivos: $TAM_TOTAL bytes"
echo "Archivos vacíos:"
echo -e "$ARCHIVOS_VACIOS"
echo "Archivo más grande:"
echo "$ARCH_MAS_GRANDE ($TAM_ARCH_MAS_GRANDE bytes)"
echo "Directorio más pequeño:"
echo "$DIR_MAS_PEQUEÑO ($TAM_DIR_MAS_PEQUEÑO bytes)"
echo "Tamaño promedio de los archivos: $TAM_PROM bytes"
```

Explicación del código:

* El código comienza declarando una serie de variables que se utilizarán para almacenar información sobre los archivos y directorios del directorio actual.
* La función `recorrerDirectorios()` se utiliza para recorrer recursivamente el directorio actual y sus subdirectorios. La función toma un argumento, que es el directorio que se va a recorrer.
* Dentro de la función `recorrerDirectorios()`, se utiliza un bucle `for` para recorrer todos los elementos del directorio especificado.
* Si el elemento es un directorio, se llama a la función `recorrerDirectorios()` de nuevo para recorrer ese directorio.
* Si el elemento es un archivo, se incrementan las variables `NUM_ARCHIVOS` y `TAM_TOTAL`, y se comprueba si el archivo está vacío o si es el archivo más grande encontrado hasta el momento.
* Después de recorrer todos los elementos del directorio actual, se calculan el tamaño promedio de los archivos y el directorio más pequeño.
* Por último, se imprimen los resultados en la pantalla.