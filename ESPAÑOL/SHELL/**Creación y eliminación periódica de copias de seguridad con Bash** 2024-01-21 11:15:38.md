```shell
#!/bin/bash

# Definir variables
DIRECTORIO_BASE=/home/usuario/directorio_base
DIRECTORIO_COPIA_SEGURIDAD=/home/usuario/directorio_copia_seguridad
FECHA_ACTUAL=$(date +%Y-%m-%d)

# Función para crear una copia de seguridad
function crear_copia_seguridad() {
  # Crear el directorio de copia de seguridad si no existe
  [ ! -d "$DIRECTORIO_COPIA_SEGURIDAD" ] && mkdir -p "$DIRECTORIO_COPIA_SEGURIDAD"

  # Copiar el directorio base en el directorio de copia de seguridad
  rsync -avz "$DIRECTORIO_BASE" "$DIRECTORIO_COPIA_SEGURIDAD/copia_seguridad_$FECHA_ACTUAL"
}

# Función para eliminar copias de seguridad antiguas
function eliminar_copias_seguridad_antiguas() {
  # Encontrar las copias de seguridad que tienen más de 30 días
  copias_seguridad_antiguas=$(find "$DIRECTORIO_COPIA_SEGURIDAD" -mtime +30 -type d)

  # Eliminar las copias de seguridad antiguas
  rm -rf $copias_seguridad_antiguas
}

# Ejecutar las funciones
crear_copia_seguridad
eliminar_copias_seguridad_antiguas

# Mostrar un mensaje de confirmación
echo "Copia de seguridad creada y copias de seguridad antiguas eliminadas."
```

Este código crea una copia de seguridad de un directorio en otro directorio, y luego elimina las copias de seguridad que tienen más de 30 días.

El código comienza definiendo las variables que se utilizarán en el script.

* `DIRECTORIO_BASE`: El directorio que se desea copiar.
* `DIRECTORIO_COPIA_SEGURIDAD`: El directorio en el que se creará la copia de seguridad.
* `FECHA_ACTUAL`: La fecha actual.

A continuación, se definen dos funciones:

* `crear_copia_seguridad()`: Esta función crea una copia de seguridad del directorio base en el directorio de copia de seguridad.
* `eliminar_copias_seguridad_antiguas()`: Esta función elimina las copias de seguridad que tienen más de 30 días.

El código luego ejecuta las dos funciones.

* `crear_copia_seguridad`: Se crea una copia de seguridad del directorio base en el directorio de copia de seguridad.
* `eliminar_copias_seguridad_antiguas()`: Se eliminan las copias de seguridad que tienen más de 30 días.

Finalmente, el código muestra un mensaje de confirmación.