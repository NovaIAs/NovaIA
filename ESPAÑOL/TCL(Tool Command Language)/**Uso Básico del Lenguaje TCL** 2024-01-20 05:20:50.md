```tcl
# Crear una lista de cadenas
lista = list "uno" "dos" "tres" "cuatro" "cinco"

# Iterar sobre la lista y mostrar cada elemento
foreach elemento $lista {
    puts "Elemento: $elemento"
}

# Crear un diccionario
diccionario = dict create uno 1 dos 2 tres 3

# Obtener un valor del diccionario
valor = dict get $diccionario uno

# Mostrar el valor
puts "Valor: $valor"

# Crear un procedimiento
proc suma {a b} {
    expr {$a + $b}
}

# Llamar al procedimiento
resultado = suma 1 2

# Mostrar el resultado
puts "Resultado: $resultado"

# Crear una ventana
wm title "Ventana TCL"
wm geometry "300x200"

# Crear un botón
button .boton -text "Clic" -command {
    puts "Se ha hecho clic en el botón"
}
pack .boton

# Entrar en el bucle de eventos
wm mainloop
```

Explicación:

* El código comienza creando una lista de cadenas utilizando el comando `list`.
* Luego, el código itera sobre la lista utilizando el comando `foreach` y muestra cada elemento utilizando el comando `puts`.
* A continuación, el código crea un diccionario utilizando el comando `dict create`.
* Luego, el código obtiene un valor del diccionario utilizando el comando `dict get`.
* El código muestra el valor utilizando el comando `puts`.
* A continuación, el código crea un procedimiento utilizando el comando `proc`.
* Luego, el código llama al procedimiento utilizando el comando `suma`.
* El código muestra el resultado utilizando el comando `puts`.
* A continuación, el código crea una ventana utilizando el comando `wm title`.
* Luego, el código crea un botón utilizando el comando `button`.
* El código añade el botón a la ventana utilizando el comando `pack`.
* Por último, el código entra en el bucle de eventos utilizando el comando `wm mainloop`.