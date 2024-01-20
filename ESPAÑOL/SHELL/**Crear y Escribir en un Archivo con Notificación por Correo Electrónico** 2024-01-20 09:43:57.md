```shell
#!/bin/bash

# Definir variables
DIRECTORIO_RAIZ=/home/usuario/Documentos
NOMBRE_ARCHIVO=archivo.txt
TEXTO="Hola mundo, este es un archivo de prueba."

# Comprobar si el directorio existe
if [ ! -d "$DIRECTORIO_RAIZ" ]; then
  # Crear el directorio si no existe
  mkdir -p "$DIRECTORIO_RAIZ"
fi

# Comprobar si el archivo existe
if [ ! -f "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO" ]; then
  # Crear el archivo si no existe
  touch "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO"
fi

# Escribir el texto en el archivo
echo "$TEXTO" > "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO"

# Imprimir un mensaje en la terminal
echo "El archivo $NOMBRE_ARCHIVO se ha creado con éxito en el directorio $DIRECTORIO_RAIZ."

# Obtener la fecha y hora actuales
FECHA_HORA=$(date +%Y-%m-%d" "%H:%M:%S)

# Añadir una entrada al log
echo "$FECHA_HORA: El archivo $NOMBRE_ARCHIVO se ha creado con éxito en el directorio $DIRECTORIO_RAIZ." >> log.txt

# Enviar un correo electrónico
# (¡Recuerda reemplazar las direcciones de correo electrónico con las tuyas!)
echo "El archivo $NOMBRE_ARCHIVO se ha creado con éxito en el directorio $DIRECTORIO_RAIZ." | mail -s "Notificación: Archivo creado" tu-correo@ejemplo.com

# Salir del script con el código de estado 0 (éxito)
exit 0
```

Explicación:

1. **Definir variables:**
   - `DIRECTORIO_RAIZ`: La ruta al directorio donde se creará el archivo.
   - `NOMBRE_ARCHIVO`: El nombre del archivo que se creará.
   - `TEXTO`: El texto que se escribirá en el archivo.

2. **Comprobar si el directorio existe:**
   - Utilizar la instrucción `if [ ! -d "$DIRECTORIO_RAIZ" ]` para comprobar si el directorio existe.
   - Si el directorio no existe, crear el directorio utilizando la instrucción `mkdir -p "$DIRECTORIO_RAIZ"`.

3. **Comprobar si el archivo existe:**
   - Utilizar la instrucción `if [ ! -f "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO" ]` para comprobar si el archivo existe.
   - Si el archivo no existe, crear el archivo utilizando la instrucción `touch "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO"`.

4. **Escribir el texto en el archivo:**
   - Utilizar la instrucción `echo "$TEXTO" > "$DIRECTORIO_RAIZ/$NOMBRE_ARCHIVO"` para escribir el texto en el archivo.

5. **Imprimir un mensaje en la terminal:**
   - Utilizar la instrucción `echo` para imprimir un mensaje en la terminal.

6. **Obtener la fecha y hora actuales:**
   - Utilizar la instrucción `date +%Y-%m-%d" "%H:%M:%S)` para obtener la fecha y hora actuales.

7. **Añadir una entrada al log:**
   - Utilizar la instrucción `echo` para añadir una entrada al archivo de registro `log.txt`.

8. **Enviar un correo electrónico:**
   - Utilizar la instrucción `echo` para enviar un correo electrónico a la dirección indicada.
   - (¡Recuerda reemplazar las direcciones de correo electrónico con las tuyas!)

9. **Salir del script:**
   - Utilizar la instrucción `exit 0` para salir del script con el código de estado 0 (éxito).