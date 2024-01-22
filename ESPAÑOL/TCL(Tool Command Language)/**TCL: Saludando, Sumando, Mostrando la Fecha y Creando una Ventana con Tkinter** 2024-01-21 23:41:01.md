```tcl
# Definimos los procedimientos a utilizar en el código

proc saludo {nombre} {
    puts "Hola, $nombre!"
}

proc sumar {num1 num2} {
    return [expr {$num1 + $num2}]
}

proc mostrarFecha {fecha} {
    puts "Hoy es $fecha."
}

# Creamos una lista con los nombres de las personas a quienes saludar

set nombres {Juan María Pedro Luisa Ana}

# Iteramos sobre la lista de nombres y llamamos al procedimiento "saludo" para cada uno

foreach nombre $nombres {
    saludo $nombre
}

# Calculamos la suma de dos números utilizando el procedimiento "sumar"

set num1 5
set num2 10
set suma [sumar $num1 $num2]
puts "La suma de $num1 y $num2 es $suma."

# Obtenemos la fecha actual y la mostramos utilizando el procedimiento "mostrarFecha"

set fecha [clock format [clock seconds] -format "%d/%m/%Y"]
mostrarFecha $fecha

# Creamos una ventana de diálogo con Tkinter y añadimos una etiqueta

package require Tk
set ventana [new Toplevel]
set etiqueta [ttk::label $ventana -text "Hola, mundo!"]
pack $etiqueta

# Ejecutamos el ciclo de eventos principal para que la ventana se muestre

mainloop
```

**Explicación del código:**

* El código comienza definiendo tres procedimientos: `saludo`, `sumar` y `mostrarFecha`.
  * El procedimiento `saludo` recibe un nombre como parámetro y muestra un mensaje de saludo.
  * El procedimiento `sumar` recibe dos números como parámetros y devuelve su suma.
  * El procedimiento `mostrarFecha` recibe una fecha como parámetro y muestra un mensaje con la fecha.

* A continuación, el código crea una lista con los nombres de las personas a quienes saludar.

* El código itera sobre la lista de nombres y llama al procedimiento `saludo` para cada nombre.

* El código calcula la suma de dos números utilizando el procedimiento `sumar`.

* El código obtiene la fecha actual y la muestra utilizando el procedimiento `mostrarFecha`.

* El código crea una ventana de diálogo con Tkinter y añade una etiqueta con el texto "Hola, mundo!".

* El código ejecuta el ciclo de eventos principal para que la ventana se muestre.