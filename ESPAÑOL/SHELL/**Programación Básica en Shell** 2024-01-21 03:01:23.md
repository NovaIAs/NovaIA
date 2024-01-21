```shell
#!/bin/bash

# Definir variables
MENSAJE="Hola, mundo!"
NUMERO=42
LISTA=(1 2 3 4 5)

# Mostrar un mensaje
echo "$MENSAJE"

# Sumar dos números
SUMA=$(($NUMERO + 5))
echo "La suma de $NUMERO y 5 es $SUMA"

# Recorrer una lista
for NUM in "${LISTA[@]}"; do
  echo "El elemento actual es $NUM"
done

# Crear un archivo
touch archivo.txt

# Escribir en un archivo
echo "Hola, mundo!" > archivo.txt

# Leer de un archivo
CONTENIDO=$(cat archivo.txt)
echo "El contenido del archivo es: $CONTENIDO"

# Comprobar si un archivo existe
if [ -f archivo.txt ]; then
  echo "El archivo existe"
else
  echo "El archivo no existe"
fi

# Eliminar un archivo
rm archivo.txt

# Crear un directorio
mkdir directorio

# Cambiar a un directorio
cd directorio

# Listar los archivos y directorios del directorio actual
ls -l

# Volver al directorio anterior
cd ..

# Ejecutar un comando
ls -l /etc

# Obtener el resultado de un comando
RESULTADO=$(ls -l /etc)
echo "El resultado del comando es: $RESULTADO"

# Comprobar si un comando tuvo éxito
if [ $? -eq 0 ]; then
  echo "El comando tuvo éxito"
else
  echo "El comando falló"
fi

# Salir del shell
exit 0
```

Explicación:

* El código comienza definiendo tres variables: `MENSAJE`, `NUMERO` y `LISTA`.
* A continuación, muestra el mensaje `Hola, mundo!` en la consola.
* Luego, suma dos números y muestra el resultado.
* Después, recorre una lista y muestra cada elemento de la lista en la consola.
* A continuación, crea un archivo llamado `archivo.txt`, escribe en él el mensaje `Hola, mundo!` y lee el contenido del archivo.
* Luego, comprueba si el archivo existe y lo elimina.
* Después, crea un directorio llamado `directorio`, cambia a ese directorio, lista los archivos y directorios del directorio actual y vuelve al directorio anterior.
* A continuación, ejecuta un comando y obtiene el resultado del comando.
* Luego, comprueba si el comando tuvo éxito y sale del shell.