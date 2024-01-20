```shell
#!/bin/bash

# Definir variables
DIRECTORIO_ACTUAL=$(pwd)
LISTA_ARCHIVOS=$(ls)
NUMERO_ARCHIVOS=$(ls | wc -l)
MAYOR_ARCHIVO=$(ls -S | head -1)
MENOR_ARCHIVO=$(ls -S | tail -1)

# Imprimir información
echo "Directorio actual: $DIRECTORIO_ACTUAL"
echo "Lista de archivos:"
echo "$LISTA_ARCHIVOS"
echo "Número de archivos: $NUMERO_ARCHIVOS"
echo "Mayor archivo: $MAYOR_ARCHIVO"
echo "Menor archivo: $MENOR_ARCHIVO"

# Crear un directorio
mkdir nuevo_directorio

# Mover archivos al nuevo directorio
mv *.txt nuevo_directorio

# Cambiar al nuevo directorio
cd nuevo_directorio

# Comprimir archivos en un archivo ZIP
zip archivos.zip *.txt

# Subir el archivo ZIP a un servidor remoto
scp archivos.zip usuario@servidor:/ruta/destino

# Regresar al directorio anterior
cd ..

# Eliminar el directorio creado
rmdir nuevo_directorio

# Mostrar mensaje de finalización
echo "Tarea completada."
```

Explicación:

1. El script comienza definiendo las variables necesarias para el procesamiento.
2. A continuación, imprime la información del directorio actual, la lista de archivos, el número de archivos, el mayor archivo y el menor archivo.
3. Luego, crea un nuevo directorio llamado "nuevo_directorio".
4. Mueve todos los archivos con extensión ".txt" al nuevo directorio.
5. Cambia al nuevo directorio.
6. Comprime todos los archivos ".txt" en un archivo ZIP llamado "archivos.zip".
7. Sube el archivo ZIP a un servidor remoto utilizando el comando "scp".
8. Regresa al directorio anterior.
9. Elimina el directorio "nuevo_directorio".
10. Finalmente, imprime un mensaje de finalización.