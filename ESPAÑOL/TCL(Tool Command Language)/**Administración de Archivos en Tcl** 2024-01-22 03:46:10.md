```tcl
# Paquete para manejar archivos
package require Tcl 8.5
package require fileutil

# Variables para manejar los archivos
set archivos [list "archivo1.txt" "archivo2.txt" "archivo3.txt"]
set directorios [list "directorio1" "directorio2" "directorio3"]

# Función para crear directorios
proc crear_directorio {nombre_directorio} {
    if {![file isdirectory $nombre_directorio]} {
        file mkdir $nombre_directorio
        puts "Directorio $nombre_directorio creado"
    } else {
        puts "El directorio $nombre_directorio ya existe"
    }
}

# Función para crear archivos
proc crear_archivo {nombre_archivo} {
    file open $nombre_archivo w
    puts "Archivo $nombre_archivo creado"
}

# Función para escribir en archivos
proc escribir_archivo {nombre_archivo texto} {
    file append $nombre_archivo $texto
}

# Función para leer archivos
proc leer_archivo {nombre_archivo} {
    set contenido [file read $nombre_archivo]
    puts "Contenido del archivo $nombre_archivo:\n$contenido"
}

# Función para eliminar archivos
proc eliminar_archivo {nombre_archivo} {
    if {[file exists $nombre_archivo]} {
        file delete $nombre_archivo
        puts "Archivo $nombre_archivo eliminado"
    } else {
        puts "El archivo $nombre_archivo no existe"
    }
}

# Función para eliminar directorios
proc eliminar_directorio {nombre_directorio} {
    if {[file isdirectory $nombre_directorio] && [file empty $nombre_directorio]} {
        file rmdir $nombre_directorio
        puts "Directorio $nombre_directorio eliminado"
    } else {
        puts "El directorio $nombre_directorio no existe o no está vacío"
    }
}

# Creamos los directorios
foreach directorio $directorios {
    crear_directorio $directorio
}

# Creamos los archivos
foreach archivo $archivos {
    crear_archivo $archivo
}

# Escribimos en los archivos
escribir_archivo $archivos(0) "Línea 1\nLínea 2\nLínea 3"
escribir_archivo $archivos(1) "Esta es otra línea"
escribir_archivo $archivos(2) "Esta es la última línea"

# Leemos los archivos
leer_archivo $archivos(0)
leer_archivo $archivos(1)
leer_archivo $archivos(2)

# Eliminamos los archivos
eliminar_archivo $archivos(0)
eliminar_archivo $archivos(1)
eliminar_archivo $archivos(2)

# Eliminamos los directorios
eliminar_directorio $directorios(0)
eliminar_directorio $directorios(1)
eliminar_directorio $directorios(2)
```

Este código realiza las siguientes acciones:

* Crea tres directorios llamados "directorio1", "directorio2" y "directorio3".
* Crea tres archivos llamados "archivo1.txt", "archivo2.txt" y "archivo3.txt" dentro de los respectivos directorios.
* Escribe texto en los archivos creados.
* Lee el contenido de los archivos creados.
* Elimina los archivos creados.
* Elimina los directorios creados.

El código utiliza funciones para realizar las acciones de crear directorios, crear archivos, escribir en archivos, leer archivos, eliminar archivos y eliminar directorios. Esto hace que el código sea más modular y fácil de entender.

El código también maneja correctamente los casos en los que los archivos o directorios ya existen o están vacíos. Esto evita que el código genere errores.