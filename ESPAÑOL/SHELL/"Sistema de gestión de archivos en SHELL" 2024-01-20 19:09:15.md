```shell
#!/bin/bash

# Crear una función para imprimir un mensaje de bienvenida
bienvenida() {
  echo "Bienvenido al sistema de gestión de archivos"
}

# Crear una función para listar los archivos de un directorio
listar_archivos() {
  echo "Archivos en el directorio actual:"
  ls -l
}

# Crear una función para crear un nuevo directorio
crear_directorio() {
  echo "Introduzca el nombre del nuevo directorio:"
  read directorio
  mkdir $directorio
  echo "Directorio creado"
}

# Crear una función para eliminar un directorio
eliminar_directorio() {
  echo "Introduzca el nombre del directorio a eliminar:"
  read directorio
  rm -r $directorio
  echo "Directorio eliminado"
}

# Crear una función para copiar un archivo de un directorio a otro
copiar_archivo() {
  echo "Introduzca el nombre del archivo a copiar:"
  read archivo
  echo "Introduzca el nombre del directorio de destino:"
  read destino
  cp $archivo $destino
  echo "Archivo copiado"
}

# Crear una función para mover un archivo de un directorio a otro
mover_archivo() {
  echo "Introduzca el nombre del archivo a mover:"
  read archivo
  echo "Introduzca el nombre del directorio de destino:"
  read destino
  mv $archivo $destino
  echo "Archivo movido"
}

# Crear una función para eliminar un archivo
eliminar_archivo() {
  echo "Introduzca el nombre del archivo a eliminar:"
  read archivo
  rm $archivo
  echo "Archivo eliminado"
}

# Crear una función para buscar un archivo
buscar_archivo() {
  echo "Introduzca el nombre del archivo a buscar:"
  read archivo
  find . -name $archivo -print
}

# Crear una función para imprimir un mensaje de despedida
despedida() {
  echo "Gracias por utilizar el sistema de gestión de archivos"
}

# Llamar a la función de bienvenida
bienvenida

# Mostrar un menú de opciones
echo "1. Listar archivos"
echo "2. Crear directorio"
echo "3. Eliminar directorio"
echo "4. Copiar archivo"
echo "5. Mover archivo"
echo "6. Eliminar archivo"
echo "7. Buscar archivo"
echo "8. Salir"

# Leer la opción del usuario
read opcion

# Ejecutar la función correspondiente a la opción seleccionada
case $opcion in
  1)
    listar_archivos
    ;;
  2)
    crear_directorio
    ;;
  3)
    eliminar_directorio
    ;;
  4)
    copiar_archivo
    ;;
  5)
    mover_archivo
    ;;
  6)
    eliminar_archivo
    ;;
  7)
    buscar_archivo
    ;;
  8)
    despedida
    exit 0
    ;;
  *)
    echo "Opción no válida"
    ;;
esac

# Volver al menú principal
bash $0
```

Explicación del código:

1. Se definen las siguientes funciones:

    * `bienvenida()`: Esta función imprime un mensaje de bienvenida al usuario.
    * `listar_archivos()`: Esta función lista los archivos del directorio actual.
    * `crear_directorio()`: Esta función crea un nuevo directorio.
    * `eliminar_directorio()`: Esta función elimina un directorio.
    * `copiar_archivo()`: Esta función copia un archivo de un directorio a otro.
    * `mover_archivo()`: Esta función mueve un archivo de un directorio a otro.
    * `eliminar_archivo()`: Esta función elimina un archivo.
    * `buscar_archivo()`: Esta función busca un archivo en el sistema.
    * `despedida()`: Esta función imprime un mensaje de despedida al usuario.

2. Se llama a la función `bienvenida()` para imprimir un mensaje de bienvenida.

3. Se muestra un menú de opciones al usuario.

4. Se lee la opción del usuario.

5. Se ejecuta la función correspondiente a la opción seleccionada.

6. Si el usuario no selecciona una opción válida, se muestra un mensaje de error.

7. Se vuelve al menú principal.

Este código es un ejemplo de cómo crear un sistema de gestión de archivos simple en shell. El código utiliza funciones para organizar el código y hacerlo más fácil de leer y mantener. El código también utiliza `case` para proporcionar una forma fácil de seleccionar la función correcta en función de la opción seleccionada por el usuario.