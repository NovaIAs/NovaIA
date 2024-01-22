```shell
#!/bin/bash

# Definir variables
DIRECTORIO_RAIZ="/home/usuario/directorio_raiz"
EXTENSIONES_PERMITIDAS="txt jpg png"
ARCHIVOS_EXCLUIDOS="archivo1.txt archivo2.png"
PATRON_FECHA="^(19|20)\d{2}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$"

# Crear función para buscar archivos
buscar_archivos() {
  find "$1" -type f -name "*.$2"
}

# Crear función para excluir archivos
excluir_archivos() {
  grep -v "$1" | grep -v "$2"
}

# Crear función para validar fechas
validar_fecha() {
  [[ "$1" =~ $PATRON_FECHA ]] && echo "Válida" || echo "Inválida"
}

# Crear función para mostrar resultados
mostrar_resultados() {
  echo "Archivos encontrados:"
  printf "%s\n" "$1"
}

# Crear una lista con las extensiones permitidas
extensiones_permitidas=($EXTENSIONES_PERMITIDAS)

# Crear una lista con los archivos excluidos
archivos_excluidos=($ARCHIVOS_EXCLUIDOS)

# Buscar archivos con las extensiones permitidas en el directorio raíz
archivos_encontrados=$(buscar_archivos "$DIRECTORIO_RAIZ" "${extensiones_permitidas[@]}")

# Excluir los archivos especificados de la lista de archivos encontrados
archivos_filtrados=$(excluir_archivos "${archivos_excluidos[@]}" <<< "$archivos_encontrados")

# Solicitar al usuario que introduzca una fecha
echo "Introduzca una fecha en formato AAAA-MM-DD:"
read fecha_introducida

# Validar la fecha introducida
fecha_valida=$(validar_fecha "$fecha_introducida")

# Mostrar los archivos encontrados que tengan una fecha de modificación anterior a la introducida
if [[ $fecha_valida == "Válida" ]]; then
  archivos_con_fecha_anterior=$(find "$DIRECTORIO_RAIZ" -type f -newermt "$fecha_introducida" -print)
  mostrar_resultados "$archivos_con_fecha_anterior"
else
  echo "La fecha introducida no es válida."
fi
```

Explicación del código:

* Se definen variables para el directorio raíz, las extensiones permitidas, los archivos excluidos y el patrón de fecha.

* Se crean funciones para buscar archivos, excluir archivos, validar fechas y mostrar resultados.

* Se crea una lista con las extensiones permitidas.

* Se crea una lista con los archivos excluidos.

* Se buscan los archivos con las extensiones permitidas en el directorio raíz.

* Se excluyen los archivos especificados de la lista de archivos encontrados.

* Se solicita al usuario que introduzca una fecha.

* Se valida la fecha introducida.

* Si la fecha introducida es válida, se muestran los archivos encontrados que tengan una fecha de modificación anterior a la introducida.

* Si la fecha introducida no es válida, se muestra un mensaje de error.