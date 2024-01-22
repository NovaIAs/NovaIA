```tcl
# Este código en TCL (Tool Command Language) es una implementación de un sistema de recomendación de películas basado en la similitud del contenido.

# Paquete para las funciones de manejo de archivos
package require Tcl 8.5
package require Tk 8.5

# Crear una lista para almacenar las películas
set peliculas {}

# Función para leer el archivo de películas y crear la lista de películas
proc importarPeliculas {archivo} {
    global peliculas

    set fp [open $archivo r]
    set linea [gets $fp]
    while {$linea != ""} {
        set pelicula [split $linea ","]
        lappend peliculas $pelicula
        set linea [gets $fp]
    }
    close $fp
}

# Función para calcular la similitud entre dos películas
proc calcularSimilitud {pelicula1 pelicula2} {
    global peliculas

    set similitud 0.0

    # Obtener las palabras clave de las dos películas
    set palabras1 [lrange $peliculas($pelicula1) 1 end]
    set palabras2 [lrange $peliculas($pelicula2) 1 end]

    # Calcular el coeficiente de similitud de Jaccard
    set interseccion [lsearch -all -exact $palabras1 $palabras2]
    set union [lsearch -all -exact $palabras1 $palabras2 -or]
    set similitud [expr {double([llength $interseccion]) / double([llength $union])}]

    return $similitud
}

# Función para recomendar películas
proc recomendarPeliculas {pelicula} {
    global peliculas

    # Obtener las palabras clave de la película
    set palabras [lrange $peliculas($pelicula) 1 end]

    # Calcular la similitud entre la película y todas las demás películas
    set similitudes {}
    for {set i 0} {$i < [llength $peliculas]} {incr i} {
        set similitud [calcularSimilitud $pelicula $i]
        lappend similitudes [list $i $similitud]
    }

    # Ordenar las películas por similitud
    lsort -index 1 -decreasing $similitudes

    # Devolver las 10 películas más similares
    return [lrange $similitudes 0 9]
}

# Crear la interfaz gráfica de usuario (GUI)
set root [Tk_MainWindow .root]
Tk_Title .root "Sistema de recomendación de películas"

# Crear una etiqueta para el título
set titulo [Tk_Label .root -text "Sistema de recomendación de películas"]
Tk_Place $titulo -x 10 -y 10

# Crear un botón para importar el archivo de películas
set botonImportar [Tk_Button .root -text "Importar archivo de películas"]
Tk_Place $botonImportar -x 10 -y 40
Tk_Command $botonImportar {
    set archivo [Tk_GetOpenFile -title "Selecciona el archivo de películas"]
    if {$archivo != ""} {
        importarPeliculas $archivo
    }
}

# Crear una lista para mostrar las películas
set listaPeliculas [Tk_Listbox .root]
Tk_Place $listaPeliculas -x 10 -y 70

# Crear un botón para recomendar películas
set botonRecomendar [Tk_Button .root -text "Recomendar películas"]
Tk_Place $botonRecomendar -x 10 -y 100
Tk_Command $botonRecomendar {
    set pelicula [Tk_GetSel $listaPeliculas]
    if {$pelicula != -1} {
        set recomendaciones [recomendarPeliculas $pelicula]
        Tk_ListboxDeleteAll $listaPeliculas
        for {set i 0} {$i < [llength $recomendaciones]} {incr i} {
            set pelicula [lindex $recomendaciones $i] 0]
            set similitud [lindex $recomendaciones $i] 1]
            Tk_ListboxInsert $listaPeliculas end "$pelicula ($similitud)"
        }
    }
}

# Ejecutar la GUI
Tk_MainLoop
```

**Explicación:**

1. **Importación de películas:** El código primero importa el archivo de películas en una lista llamada `peliculas`. Cada película está representada por una lista de palabras clave.
2. **Cálculo de la similitud:** La función `calcularSimilitud` calcula la similitud entre dos películas utilizando el coeficiente de similitud de Jaccard.
3. **Recomendación de películas:** La función `recomendarPeliculas` toma una película como entrada y devuelve una lista de las 10 películas más similares.
4. **Interfaz gráfica de usuario (GUI):** El código crea una GUI utilizando la biblioteca Tk. La GUI incluye:
    * Un título
    * Un botón para importar el archivo de películas
    * Una lista para mostrar las películas
    * Un botón para recomendar películas
5. **Ejecución de la GUI:** El código ejecuta la GUI utilizando el comando `Tk_MainLoop`.