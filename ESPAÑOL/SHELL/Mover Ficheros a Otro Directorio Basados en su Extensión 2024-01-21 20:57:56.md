```shell
#!/bin/bash

# VARIABLES GLOBALES
FICHERO_TEMPORAL=$(mktemp)
DIRECTORIO_TEMPORAL=$(mktemp -d)
DIRECTORIO_DESTINO="/ruta/a/destino"
EXTENSIONES_PERMITIDAS=(jpg jpeg png gif)

# FUNCIÓN PARA VALIDAR LA EXTENSIÓN DE UN FICHERO
function es_extension_permitida() {
    local fichero="$1"
    local extension="${fichero##*.}"
    for ext in "${EXTENSIONES_PERMITIDAS[@]}"; do
        if [[ "$ext" == "$extension" ]]; then
            return 0
        fi
    done
    return 1
}

# FUNCIÓN PARA COPIAR UN FICHERO A UN DIRECTORIO TEMPORAL
function copiar_fichero_temporal() {
    local fichero="$1"
    cp "$fichero" "$FICHERO_TEMPORAL"
}

# FUNCIÓN PARA MOVER UN FICHERO DEL DIRECTORIO TEMPORAL AL DIRECTORIO DE DESTINO
function mover_fichero_destino() {
    mv "$FICHERO_TEMPORAL" "$DIRECTORIO_DESTINO/$RANDOM"
}

# FUNCIÓN PRINCIPAL
function main() {
    # RECORRER LOS FICHEROS DEL DIRECTORIO ACTUAL
    for fichero in *; do
        # SI ES UN FICHERO Y SU EXTENSIÓN ESTÁ PERMITIDA, COPIARLO AL DIRECTORIO TEMPORAL
        if [[ -f "$fichero" && es_extension_permitida "$fichero" ]]; then
            copiar_fichero_temporal "$fichero"

            # MOVER EL FICHERO DEL DIRECTORIO TEMPORAL AL DIRECTORIO DE DESTINO
            mover_fichero_destino
        fi
    done

    # ELIMINAR EL DIRECTORIO TEMPORAL Y EL FICHERO TEMPORAL
    rm -rf "$DIRECTORIO_TEMPORAL"
    rm "$FICHERO_TEMPORAL"
}

# LLAMADA A LA FUNCIÓN PRINCIPAL
main
```

Explicación:

* El código comienza definiendo las variables globales necesarias para el funcionamiento del script.
* La función `es_extension_permitida()` comprueba si la extensión de un fichero está incluida en la lista de extensiones permitidas.
* La función `copiar_fichero_temporal()` copia un fichero al directorio temporal.
* La función `mover_fichero_destino()` mueve un fichero del directorio temporal al directorio de destino.
* La función `main()` recorre los ficheros del directorio actual y, si son ficheros con una extensión permitida, los copia al directorio temporal y luego los mueve al directorio de destino.
* Al final, se eliminan el directorio temporal y el fichero temporal.