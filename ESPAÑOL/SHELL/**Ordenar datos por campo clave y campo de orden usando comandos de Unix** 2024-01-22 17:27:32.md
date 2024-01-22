```shell
#!/bin/bash

# Definiciones de variables
fichero_entrada="datos.txt"
fichero_salida="resultados.txt"
campo_separador=","
campo_clave="nombre"
campo_orden="edad"

# Leer el fichero de entrada y extraer los campos clave y de orden
while IFS="$campo_separador" read -r nombre edad; do
  echo "$nombre $edad" >> "$fichero_clave"
  echo "$edad $nombre" >> "$fichero_orden"
done < "$fichero_entrada"

# Ordenar los campos clave y de orden
sort -k1,1n "$fichero_clave" > "$fichero_clave.ordenado"
sort -k1,1n "$fichero_orden" > "$fichero_orden.ordenado"

# Unir los campos clave y de orden y escribirlos en el fichero de salida
join -t ' ' "$fichero_clave.ordenado" "$fichero_orden.ordenado" > "$fichero_salida"

# Eliminar los ficheros temporales
rm "$fichero_clave" "$fichero_orden" "$fichero_clave.ordenado" "$fichero_orden.ordenado"
```

Explicación del código:

* El código comienza leyendo el fichero de entrada y extrayendo los campos clave y de orden. El fichero de entrada está separado por comas y se espera que tenga dos campos: nombre y edad.
* Los campos clave y de orden se escriben en dos ficheros temporales, `fichero_clave` y `fichero_orden`, respectivamente.
* Los ficheros temporales se ordenan según el primer campo, que es el campo clave o el campo de orden.
* Una vez ordenados los ficheros temporales, se unen mediante la utilidad `join` y el resultado se escribe en el fichero de salida.
* Por último, se eliminan los ficheros temporales.

El código es complejo porque utiliza varias utilidades de Unix y realiza varias operaciones diferentes. Sin embargo, el código está bien organizado y es fácil de seguir.