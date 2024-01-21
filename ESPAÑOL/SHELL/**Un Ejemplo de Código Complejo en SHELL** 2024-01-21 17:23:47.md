```sh
#!/bin/bash

# Este script es un ejemplo de un código complejo en SHELL.

# Definimos algunas variables globales.
VARIABLES="NOMBRE APELLIDO EDAD"

# Creamos un bucle for para recorrer las variables globales.
for VARIABLE in $VARIABLES; do
  # Mostramos el valor de la variable actual.
  echo "Valor de la variable $VARIABLE: ${!VARIABLE}"
done

# Creamos una función para calcular el promedio de una lista de números.
function promedio() {
  # Obtenemos los números de la entrada estándar.
  NUMEROS=$(cat)

  # Calculamos el promedio de los números.
  PROMEDIO=$(echo "$NUMEROS" | awk '{ sum += $1; n++ } END { if (n > 0) print sum / n }')

  # Mostramos el promedio.
  echo "Promedio: $PROMEDIO"
}

# Llamamos a la función `promedio`.
promedio

# Creamos un bucle while para leer líneas de la entrada estándar hasta que se ingrese una línea vacía.
while read LINEA; do
  # Si la línea está vacía, salimos del bucle.
  if [ -z "$LINEA" ]; then
    break
  fi

  # Mostramos la línea actual.
  echo "Línea actual: $LINEA"
done

# Mostramos un mensaje de despedida.
echo "Gracias por usar este script."
```

El código anterior es un ejemplo de un código complejo en SHELL. El código está dividido en varias secciones, cada una de las cuales realiza una tarea específica.

La primera sección del código define algunas variables globales. Estas variables se utilizan en el resto del código para almacenar información sobre el usuario.

La segunda sección del código crea un bucle for para recorrer las variables globales. El bucle for muestra el valor de cada variable global en la salida estándar.

La tercera sección del código crea una función para calcular el promedio de una lista de números. La función utiliza el comando `awk` para calcular el promedio de los números que se le pasan como entrada.

La cuarta sección del código llama a la función `promedio` para calcular el promedio de una lista de números que se leen de la entrada estándar.

La quinta sección del código crea un bucle while para leer líneas de la entrada estándar hasta que se ingrese una línea vacía. El bucle while muestra cada línea en la salida estándar.

La sexta sección del código muestra un mensaje de despedida.