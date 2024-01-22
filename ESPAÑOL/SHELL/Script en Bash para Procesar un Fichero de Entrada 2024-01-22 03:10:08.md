```shell
#!/bin/bash

# Función para mostrar un mensaje de ayuda
mostrar_ayuda() {
  echo "Uso: $0 [opciones] <fichero>"
  echo "Opciones:"
  echo "  -h, --help       Mostrar este mensaje de ayuda"
  echo "  -v, --verbose    Mostrar información detallada sobre el proceso"
  echo "  -o, --output <fichero>    Especificar el fichero de salida"
}

# Función para procesar los argumentos de la línea de comandos
procesar_argumentos() {
  while [[ $# -gt 0 ]]; do
    case $1 in
      -h|--help)
        mostrar_ayuda
        exit 0
        ;;
      -v|--verbose)
        verbose=true
        ;;
      -o|--output)
        output=$2
        shift
        ;;
      -*)
        echo "Opción desconocida: $1" >&2
        exit 1
        ;;
      *)
        fichero=$1
        ;;
    esac
    shift
  done

  if [[ -z $fichero ]]; then
    echo "Fichero de entrada no especificado" >&2
    exit 1
  fi
}

# Función para procesar el fichero de entrada
procesar_fichero() {
  if [[ ! -f $fichero ]]; then
    echo "Fichero de entrada no encontrado: $fichero" >&2
    exit 1
  fi

  if [[ -z $output ]]; then
    output="/dev/stdout"
  fi

  while read linea; do
    if [[ $verbose ]]; then
      echo "Procesando línea: $linea"
    fi

    # Procesar la línea aquí

    if [[ $? -ne 0 ]]; then
      echo "Error al procesar la línea: $linea" >&2
      exit 1
    fi
  done < $fichero > $output
}

# Función principal
main() {
  procesar_argumentos "$@"
  procesar_fichero
}

# Llamar a la función principal
main "$@"
```

Explicación del código:

* La primera línea es la "shebang", que le dice al sistema operativo qué intérprete usar para ejecutar el script. En este caso, estamos usando el intérprete Bash.
* La segunda línea es un comentario que describe brevemente el propósito del script.
* Las siguientes líneas definen tres funciones: `mostrar_ayuda()`, `procesar_argumentos()` y `procesar_fichero()`. Estas funciones se explican a continuación.
* La función `mostrar_ayuda()` muestra un mensaje de ayuda que describe el uso del script y las opciones disponibles.
* La función `procesar_argumentos()` procesa los argumentos de la línea de comandos y establece las variables necesarias para el script.
* La función `procesar_fichero()` procesa el fichero de entrada y escribe el resultado en el fichero de salida.
* La siguiente línea llama a la función `main()` con los argumentos de la línea de comandos. La función `main()` llama a las otras funciones para realizar el trabajo real del script.
* La última línea es una llamada a la función `main()`, que inicia la ejecución del script.

Este código es un ejemplo de un script Bash complejo que procesa un fichero de entrada y escribe el resultado en un fichero de salida. El script puede ser personalizado para realizar cualquier tarea que sea necesaria.