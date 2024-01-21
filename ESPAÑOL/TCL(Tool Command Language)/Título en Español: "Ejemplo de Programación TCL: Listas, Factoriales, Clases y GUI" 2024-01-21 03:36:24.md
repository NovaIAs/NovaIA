```tcl
# Crear una lista de números del 1 al 10
numeros = [list 10]

# Iterar sobre la lista de números y mostrarlos en pantalla
foreach numero $numeros {
    puts "Número: $numero"
}

# Definir una función para calcular el factorial de un número
proc factorial {numero} {
    if {$numero == 0} {
        return 1
    } else {
        return [expr {$numero * [factorial [incr $numero -1]]}]
    }
}

# Obtener el factorial de 5 y mostrarlo en pantalla
puts "Factorial de 5: [factorial 5]"

# Definir una clase para representar un punto en el espacio
class Punto {
    variable x # Coordenada X
    variable y # Coordenada Y

    # Constructor de la clase
    method __init__ {this x y} {
        set this.x $x
        set this.y $y
    }

    # Método para mostrar el punto en pantalla
    method show {this} {
        puts "Punto ($this.x, $this.y)"
    }
}

# Crear una instancia de la clase Punto
punto = [new Punto 10 20]

# Mostrar el punto en pantalla
[punto show]

# Definir una interfaz gráfica de usuario (GUI)
toplevel .main {
    button .btn_salir -text "Salir" -command {exit}
    pack .btn_salir
}

# Iniciar la GUI
tk main
```

Explicación del código:

* La primera parte del código crea una lista de números del 1 al 10 y la itera, mostrando cada número en pantalla.
* La segunda parte define una función para calcular el factorial de un número y la utiliza para calcular el factorial de 5.
* La tercera parte define una clase para representar un punto en el espacio y crea una instancia de la clase.
* La cuarta parte define una interfaz gráfica de usuario (GUI) con un botón de salida y la inicia.

Este código es un ejemplo de lo que se puede hacer con Tcl. Tcl es un lenguaje de scripting muy potente y versátil que se puede utilizar para crear una amplia variedad de aplicaciones.