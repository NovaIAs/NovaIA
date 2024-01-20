```
#!/bin/bash

# 1. VARIABLES Y PARÁMETROS

# Defina una variable de cadena
nombre="Juan"

# Defina una variable de número entero
edad=30

# Defina una variable de matriz
frutas=("manzana" "naranja" "pera")

# Obtenga el primer parámetro de la línea de comandos
primer_parametro="$1"

# 2. CONDICIONALES

# Comprueba si el nombre es "Juan"
if [ "$nombre" == "Juan" ]; then
  echo "El nombre es Juan"
else
  echo "El nombre no es Juan"
fi

# Comprueba si la edad es mayor o igual a 18
if [ "$edad" -ge 18 ]; then
  echo "La edad es mayor o igual a 18"
else
  echo "La edad es menor a 18"
fi

# Comprueba si el primer parámetro es vacío
if [ -z "$primer_parametro" ]; then
  echo "El primer parámetro es vacío"
else
  echo "El primer parámetro es $primer_parametro"
fi

# 3. BUCLES

# Bucle for para recorrer una matriz
for fruta in "${frutas[@]}"; do
  echo "Fruta: $fruta"
done

# Bucle while para recorrer una lista de números
contador=1
while [ $contador -le 10 ]; do
  echo "Número: $contador"
  ((contador++))
done

# 4. FUNCIONES

# Defina una función para saludar
hola() {
  echo "Hola, $1!"
}

# Llame a la función hola
hola "Juan"

# 5. ARCHIVOS Y DIRECTORIOS

# Cree un archivo llamado "archivo.txt"
touch archivo.txt

# Escriba una línea en el archivo "archivo.txt"
echo "Hola, mundo!" > archivo.txt

# Lea una línea del archivo "archivo.txt"
linea=$(head -n 1 archivo.txt)

# Imprima la línea leída
echo "Línea leída: $linea"

# Elimine el archivo "archivo.txt"
rm archivo.txt

# 6. COMANDOS DEL SISTEMA

# Obtenga la lista de directorios en el directorio actual
directorios=$(ls -d */)

# Imprima la lista de directorios
echo "Directorios:"
echo "$directorios"

# Cree un nuevo directorio llamado "nuevo_directorio"
mkdir nuevo_directorio

# Cambie al directorio "nuevo_directorio"
cd nuevo_directorio

# Cree un nuevo archivo llamado "archivo_nuevo.txt"
touch archivo_nuevo.txt

# Escriba una línea en el archivo "archivo_nuevo.txt"
echo "Hola, nuevo mundo!" > archivo_nuevo.txt

# Lea una línea del archivo "archivo_nuevo.txt"
linea_nueva=$(head -n 1 archivo_nuevo.txt)

# Imprima la línea leída
echo "Línea leída: $linea_nueva"

# Elimine el archivo "archivo_nuevo.txt"
rm archivo_nuevo.txt

# Cambie al directorio anterior
cd ..

# Elimine el directorio "nuevo_directorio"
rmdir nuevo_directorio

# 7. ALIASES

# Defina un alias para el comando "ls"
alias ls="ls -lah"

# Use el alias para listar los archivos y directorios en el directorio actual
ls

# 8. OTRAS CARACTERÍSTICAS

# Utilice comillas dobles para permitir la expansión de variables
nombre_completo="Juan Pérez"
echo "Nombre completo: $nombre_completo"

# Utilice comillas simples para evitar la expansión de variables
nombre_completo='Juan Pérez'
echo "Nombre completo: $nombre_completo"

# Utilice el signo de dólar para obtener el PID del proceso actual
echo "PID del proceso actual: $$"

# Utilice el signo de exclamación para ejecutar un comando en segundo plano
sleep 10 &

# Utilice el signo de canalización para conectar la salida de un comando a la entrada de otro
ls | grep "archivo"

# Utilice el signo de punto y coma para ejecutar varios comandos en una sola línea
ls; pwd

```

Explicación:

* Este código es un script de shell complejo que demuestra varias características del shell, incluyendo variables, parámetros, condicionales, bucles, funciones, archivos y directorios, comandos del sistema, alias y otras características.
* El script define varias variables, incluyendo una variable de cadena, una variable de número entero y una variable de matriz.
* También obtiene el primer parámetro de la línea de comandos.
* El script utiliza condicionales para comprobar el valor de las variables y el primer parámetro.
* Utiliza bucles para recorrer una matriz y una lista de números.
* Define una función para saludar y la llama.
* Trabaja con archivos y directorios, incluyendo la creación, lectura, escritura y eliminación de archivos y directorios.
* Utiliza comandos del sistema para obtener la lista de directorios en el directorio actual, crear un nuevo directorio, cambiar al directorio "nuevo_directorio", crear un nuevo archivo en ese directorio, leer una línea del archivo, imprimir la línea leída y eliminar el archivo.
* El script también define un alias para el comando "ls" y lo utiliza para listar los archivos y directorios en el directorio actual.
* Por último, el script demuestra el uso de comillas dobles, comillas simples, el signo de dólar, el signo de exclamación, el signo de canalización y el signo de punto y coma.