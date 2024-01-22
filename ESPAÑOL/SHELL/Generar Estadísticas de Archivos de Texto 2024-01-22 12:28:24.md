```shell
#!/bin/bash

# Crear un script que tome un archivo de texto y genere una estadística con la cantidad de palabras, líneas y caracteres.

# Función para contar las palabras en un archivo de texto.
contar_palabras() {
  local archivo=$1
  local palabras=0
  while IFS= read -r línea; do
    palabras=$((palabras + $(echo $línea | wc -w)))
  done < $archivo
  echo $palabras
}

# Función para contar las líneas en un archivo de texto.
contar_líneas() {
  local archivo=$1
  local líneas=0
  while IFS= read -r línea; do
    líneas=$((líneas + 1))
  done < $archivo
  echo $líneas
}

# Función para contar los caracteres en un archivo de texto.
contar_caracteres() {
  local archivo=$1
  local caracteres=0
  while IFS= read -r línea; do
    caracteres=$((caracteres + $(echo $línea | wc -c)))
  done < $archivo
  echo $caracteres
}

# Función para generar una estadística con la cantidad de palabras, líneas y caracteres en un archivo de texto.
generar_estadística() {
  local archivo=$1
  local palabras=$(contar_palabras $archivo)
  local líneas=$(contar_líneas $archivo)
  local caracteres=$(contar_caracteres $archivo)
  echo "Estadística del archivo $archivo:"
  echo "Palabras: $palabras"
  echo "Líneas: $líneas"
  echo "Caracteres: $caracteres"
}

# Leer el archivo de texto desde el argumento de la línea de comandos.
archivo=$1

# Generar la estadística del archivo de texto.
generar_estadística $archivo
```

Explicación del código:

1. Creamos una función llamada `contar_palabras` que toma un archivo de texto como argumento y cuenta la cantidad de palabras en ese archivo.
2. Creamos una función llamada `contar_líneas` que toma un archivo de texto como argumento y cuenta la cantidad de líneas en ese archivo.
3. Creamos una función llamada `contar_caracteres` que toma un archivo de texto como argumento y cuenta la cantidad de caracteres en ese archivo.
4. Creamos una función llamada `generar_estadística` que toma un archivo de texto como argumento y genera una estadística con la cantidad de palabras, líneas y caracteres en ese archivo.
5. Leemos el archivo de texto desde el argumento de la línea de comandos.
6. Generamos la estadística del archivo de texto usando la función `generar_estadística`.

Este código es complejo porque utiliza varias funciones y tiene un flujo de ejecución más complejo que un script simple. Sin embargo, es un ejemplo de cómo se puede utilizar el shell para crear programas más sofisticados.