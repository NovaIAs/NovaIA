```bash
#!/bin/bash

# Definición de variables
fichero_entrada="archivo_entrada.txt"
fichero_salida="archivo_salida.txt"
linea_a_buscar="cadena_a_buscar"
linea_de_sustitución="nueva_cadena"

# Verificar si el fichero de entrada existe
if [ ! -f "$fichero_entrada" ]; then
  echo "El fichero de entrada no existe. Saliendo..."
  exit 1
fi

# Verificar si el fichero de salida existe
if [ -f "$fichero_salida" ]; then
  echo "El fichero de salida ya existe. ¿Desea sobrescribirlo? (s/n)"
  read respuesta
  if [ "$respuesta" != "s" ]; then
    echo "Saliendo..."
    exit 1
  fi
fi

# Crear el fichero de salida
touch "$fichero_salida"

# Buscar y sustituir la línea en el fichero de entrada
while IFS= read -r linea; do
  if [[ "$linea" == "$linea_a_buscar" ]]; then
    echo "$linea_de_sustitución" >> "$fichero_salida"
  else
    echo "$linea" >> "$fichero_salida"
  fi
done < "$fichero_entrada"

# Mostrar mensaje de éxito
echo "La línea '$linea_a_buscar' ha sido sustituida por '$linea_de_sustitución' en el fichero '$fichero_salida'."

# Fin del script
```

Explicación del código:

* **Líneas 1-4:** Definición de variables.
* **Líneas 6-11:** Verificar si el fichero de entrada existe. Si no existe, se muestra un mensaje de error y se sale del script.
* **Líneas 13-20:** Verificar si el fichero de salida existe. Si existe, se pide al usuario si desea sobrescribirlo. Si la respuesta es "n", se sale del script.
* **Línea 22:** Crear el fichero de salida.
* **Líneas 24-35:** Buscar y sustituir la línea en el fichero de entrada. Se utiliza un bucle `while` para leer cada línea del fichero de entrada. Si la línea coincide con la cadena a buscar, se escribe la línea de sustitución en el fichero de salida. De lo contrario, se escribe la línea original en el fichero de salida.
* **Líneas 37-39:** Mostrar mensaje de éxito.
* **Línea 41:** Fin del script.