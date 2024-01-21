```
#!/bin/bash

# Se definen las variables globales que se utilizarán en el script.
NOMBRE_SCRIPT=$(basename "$0")
AYUDA=$(cat <<EOF
Uso: $NOMBRE_SCRIPT [OPCIONES] [ARGUMENTOS]

Opciones:
  -h, --help             Muestra esta ayuda y sale.
  -v, --version          Muestra la versión del script y sale.
  -f, --force            Fuerza la ejecución del script sin preguntar al usuario.

Argumentos:
  nombre_fichero         Nombre del fichero a procesar.
  nombre_directorio      Nombre del directorio donde se encuentra el fichero.

Ejemplo:
  $NOMBRE_SCRIPT -v
  $NOMBRE_SCRIPT -f nombre_fichero nombre_directorio
EOF
)
VERSION="1.0"

# Se procesan los argumentos del script.
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h | --help)
      echo "$AYUDA"
      exit 0
      ;;
    -v | --version)
      echo "$NOMBRE_SCRIPT $VERSION"
      exit 0
      ;;
    -f | --force)
      FORCE=true
      shift
      ;;
    *)
      if [[ -z "$NOMBRE_FICHERO" ]]; then
        NOMBRE_FICHERO="$1"
      elif [[ -z "$NOMBRE_DIRECTORIO" ]]; then
        NOMBRE_DIRECTORIO="$1"
      else
        echo "Error: Se han proporcionado demasiados argumentos."
        echo "$AYUDA"
        exit 1
      fi
      shift
      ;;
  esac
done

# Se comprueba si se han proporcionado todos los argumentos necesarios.
if [[ -z "$NOMBRE_FICHERO" || -z "$NOMBRE_DIRECTORIO" ]]; then
  echo "Error: Se deben proporcionar todos los argumentos necesarios."
  echo "$AYUDA"
  exit 1
fi

# Se comprueba si el fichero existe.
if [[ ! -f "$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO" ]]; then
  echo "Error: El fichero \"$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO\" no existe."
  exit 1
fi

# Se comprueba si el usuario quiere forzar la ejecución del script.
if [[ -z "$FORCE" ]]; then
  echo "¿Está seguro de que quiere procesar el fichero \"$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO\"? (s/n)"
  read CONFIRMACION
  if [[ "$CONFIRMACION" != "s" ]]; then
    echo "Cancelando el script."
    exit 0
  fi
fi

# Se procesa el fichero.
echo "Procesando el fichero \"$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO\"..."

# Se obtienen las líneas del fichero y se almacenan en un array.
LINEAS=$(cat "$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO")

# Se recorren las líneas del fichero y se procesan.
for LINEA in $LINEAS; do
  # Se procesa la línea y se obtiene el resultado.
  RESULTADO=$(procesar_linea "$LINEA")

  # Se imprime el resultado.
  echo "$RESULTADO"
done

# Se finaliza el script.
echo "El fichero \"$NOMBRE_DIRECTORIO/$NOMBRE_FICHERO\" se ha procesado correctamente."
```

**Explicación del código:**

* El script comienza definiendo las variables globales que se utilizarán en el mismo.
* A continuación, se procesan los argumentos del script.
* Se comprueba si se han proporcionado todos los argumentos necesarios.
* Se comprueba si el fichero existe.
* Se comprueba si el usuario quiere forzar la ejecución del script.
* Se procesa el fichero.
* Se finaliza el script.

**Funciones:**

* `procesar_linea()`: Esta función procesa una línea del fichero y devuelve el resultado.

**Ejemplo de uso:**

```
./script.sh -f nombre_fichero nombre_directorio
```

Este comando procesará el fichero `nombre_fichero` que se encuentra en el directorio `nombre_directorio`.