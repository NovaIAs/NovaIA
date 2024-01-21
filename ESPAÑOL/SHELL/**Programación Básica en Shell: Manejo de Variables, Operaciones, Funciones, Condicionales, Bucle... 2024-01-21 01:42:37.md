```shell
#!/bin/bash

# DECLARACIÓN DE VARIABLES
contador=0
suma=0
promedio=0
numeros=(1 2 3 4 5 6 7 8 9 10)

# RECORRER LOS NÚMEROS Y REALIZAR OPERACIONES
for numero in "${numeros[@]}"; do
  # INCREMENTAR EL CONTADOR
  contador=$((contador + 1))

  # SUMAR EL NÚMERO A LA SUMA TOTAL
  suma=$((suma + numero))

  # CALCULAR EL PROMEDIO
  promedio=$(echo "$suma / $contador" | bc -l)
done

# IMPRIMIR LOS RESULTADOS
echo "Contador: $contador"
echo "Suma: $suma"
echo "Promedio: $promedio"

# FUNCIONES
function maximo() {
  local maximo=$1

  for numero in "${@:2}"; do
    if [[ $numero -gt $maximo ]]; then
      maximo=$numero
    fi
  done

  echo "$maximo"
}

function minimo() {
  local minimo=$1

  for numero in "${@:2}"; do
    if [[ $numero -lt $minimo ]]; then
      minimo=$numero
    fi
  done

  echo "$minimo"
}

# LLAMAR A LAS FUNCIONES
maximo_numero=$(maximo "${numeros[@]}")
minimo_numero=$(minimo "${numeros[@]}")

# IMPRIMIR LOS RESULTADOS
echo "Número máximo: $maximo_numero"
echo "Número mínimo: $minimo_numero"

# CONDICIONAL
if [[ $contador -gt 5 ]]; then
  echo "El contador es mayor que 5"
elif [[ $contador -lt 5 ]]; then
  echo "El contador es menor que 5"
else
  echo "El contador es igual a 5"
fi

# BUCLE WHILE
while [[ $contador -gt 0 ]]; do
  echo "El contador es $contador"

  # DECREMENTAR EL CONTADOR
  contador=$((contador - 1))
done

# BUCLE UNTIL
until [[ $contador -eq 10 ]]; do
  echo "El contador es $contador"

  # INCREMENTAR EL CONTADOR
  contador=$((contador + 1))
done

# CASO PRÁCTICO: GESTIÓN DE FICHEROS
# CREAR UN FICHERO
touch fichero.txt

# ESCRIBIR EN UN FICHERO
echo "Hola mundo" > fichero.txt

# LEER UN FICHERO
contenido=$(cat fichero.txt)

# IMPRIMIR EL CONTENIDO DEL FICHERO
echo "Contenido del fichero:"
echo "$contenido"

# ELIMINAR UN FICHERO
rm fichero.txt

# CASO PRÁCTICO: GESTIÓN DE DIRECTORIOS
# CREAR UN DIRECTORIO
mkdir directorio

# CAMBIAR EL DIRECTORIO ACTUAL
cd directorio

# CREAR UN FICHERO EN EL DIRECTORIO ACTUAL
touch fichero.txt

# LEER EL CONTENIDO DEL DIRECTORIO ACTUAL
contenido=$(ls)

# IMPRIMIR EL CONTENIDO DEL DIRECTORIO ACTUAL
echo "Contenido del directorio:"
echo "$contenido"

# ELIMINAR EL FICHERO DEL DIRECTORIO ACTUAL
rm fichero.txt

# ELIMINAR EL DIRECTORIO ACTUAL
rmdir directorio

# CASO PRÁCTICO: PROCESAMIENTO DE CADENAS
# OBTENER LA LONGITUD DE UNA CADENA
longitud=$(echo "Hola mundo" | wc -c)

# IMPRIMIR LA LONGITUD DE LA CADENA
echo "Longitud de la cadena: $longitud"

# OBTENER UNA SUBCADENA
subcadena=$(echo "Hola mundo" | cut -c 1-5)

# IMPRIMIR LA SUBCADENA
echo "Subcadena: $subcadena"

# REEMPLAZAR UNA SUBCADENA
cadena_reemplazada=$(echo "Hola mundo" | sed 's/mundo/universo/')

# IMPRIMIR LA CADENA REEMPLAZADA
echo "Cadena reemplazada: $cadena_reemplazada"

# CASO PRÁCTICO: PROCESAMIENTO DE LISTAS
# OBTENER EL PRIMER ELEMENTO DE UNA LISTA
primer_elemento="${numeros[0]}"

# IMPRIMIR EL PRIMER ELEMENTO DE LA LISTA
echo "Primer elemento de la lista: $primer_elemento"

# OBTENER EL ÚLTIMO ELEMENTO DE UNA LISTA
ultimo_elemento="${numeros[-1]}"

# IMPRIMIR EL ÚLTIMO ELEMENTO DE LA LISTA
echo "Último elemento de la lista: $ultimo_elemento"

# AÑADIR UN ELEMENTO A UNA LISTA
numeros+=("11")

# IMPRIMIR LA LISTA CON EL ELEMENTO AÑADIDO
echo "Lista con el elemento añadido:"
echo "${numeros[@]}"

# ELIMINAR UN ELEMENTO DE UNA LISTA
unset numeros[0]

# IMPRIMIR LA LISTA CON EL ELEMENTO ELIMINADO
echo "Lista con el elemento eliminado:"
echo "${numeros[@]}"

```

Explicación del código:

* Se declaran variables para almacenar el contador, la suma, el promedio, el número máximo, el número mínimo y los números.
* Se utiliza un bucle `for` para recorrer los números y realizar operaciones sobre ellos.
* Se utilizan las funciones `maximo()` y `minimo()` para encontrar el número máximo y el número mínimo, respectivamente.
* Se utiliza un condicional `if` para comprobar si el contador es mayor, menor o igual que 5.
* Se utilizan bucles `while` y `until` para demostrar la iteración.
* Se utilizan comandos de gestión de ficheros y directorios para crear, leer, escribir y eliminar ficheros y directorios.
* Se emplean comandos de procesamiento de cadenas para obtener la longitud de una cadena, obtener una subcadena y reemplazar una subcadena.
* Se utilizan comandos de procesamiento de listas para obtener el primer elemento, el último elemento, añadir un elemento y eliminar un elemento de una lista.