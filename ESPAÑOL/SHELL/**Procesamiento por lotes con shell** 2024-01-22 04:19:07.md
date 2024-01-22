```shell
#!/bin/bash

# Definir variables
fichero_configuracion="/ruta/a/fichero_configuracion.conf"
fichero_registros="/ruta/a/fichero_registros.log"
fichero_salida="/ruta/a/fichero_salida.txt"
comando="algun-comando"

# Leer el fichero de configuración
configuracion=$(cat $fichero_configuracion)

# Procesar la configuración
for linea in $configuracion; do
  # Dividir la línea en clave y valor
  clave=${linea%%=*}
  valor=${linea#*=}

  # Procesar la clave y el valor
  case $clave in
    "clave1")
      # Hacer algo con el valor de clave1
      ;;
    "clave2")
      # Hacer algo con el valor de clave2
      ;;
    *)
      # Ignorar la línea
      ;;
  esac
done

# Ejecutar el comando
salida=$(eval $comando)

# Escribir la salida en el fichero de salida
echo "$salida" > $fichero_salida

# Escribir un mensaje en el fichero de registros
echo "Se ha ejecutado el comando $comando" >> $fichero_registros
```

Explicación del código:

1. Definir variables: Se definen las variables que se utilizan en el script, incluyendo el fichero de configuración, el fichero de registros, el fichero de salida y el comando que se va a ejecutar.
2. Leer el fichero de configuración: Se lee el fichero de configuración y se guarda el contenido en la variable `configuracion`.
3. Procesar la configuración: Se procesa la configuración leyendo cada línea del fichero y dividiéndola en clave y valor. Luego se procesa cada clave y valor según su valor.
4. Ejecutar el comando: Se ejecuta el comando especificado en la variable `comando` y se guarda la salida en la variable `salida`.
5. Escribir la salida en el fichero de salida: Se escribe la salida del comando en el fichero de salida especificado en la variable `fichero_salida`.
6. Escribir un mensaje en el fichero de registros: Se escribe un mensaje en el fichero de registros indicando que se ha ejecutado el comando.