```tcl
# Definir una lista de nombres de personas.
nombres = {Juan, María, Pedro, Ana, Luis}

# Crear una función para saludar a una persona.
proc saludar {nombre} {
    puts "Hola, $nombre!"
}

# Iterar sobre la lista de nombres y saludar a cada persona.
foreach nombre $nombres {
    saludar $nombre
}

# Crear una matriz de datos de personas.
personas = {
    {nombre Juan edad 20},
    {nombre María edad 25},
    {nombre Pedro edad 30},
    {nombre Ana edad 35},
    {nombre Luis edad 40}
}

# Iterar sobre la matriz de personas e imprimir sus datos.
foreach persona $personas {
    puts "Nombre: $persona(nombre), Edad: $persona(edad)"
}

# Crear una función para calcular el promedio de edad de las personas.
proc promedio_edad {personas} {
    set suma_edades 0
    foreach persona $personas {
        set suma_edades [expr {$suma_edades + $persona(edad)}]
    }
    set promedio [expr {$suma_edades / [llength $personas]}]
    return $promedio
}

# Calcular el promedio de edad de las personas.
set promedio_edad [promedio_edad $personas]
puts "El promedio de edad de las personas es: $promedio_edad"
```

Explicación:

* El código comienza definiendo una lista de nombres de personas.
* Luego, se crea una función llamada `saludar` que toma un nombre como argumento y lo saluda.
* Se utiliza un bucle `foreach` para iterar sobre la lista de nombres y saludar a cada persona.
* A continuación, se crea una matriz de datos de personas, donde cada elemento de la matriz es un diccionario que contiene el nombre y la edad de una persona.
* Se utiliza otro bucle `foreach` para iterar sobre la matriz de personas e imprimir sus datos.
* Luego, se crea una función llamada `promedio_edad` que toma una matriz de personas como argumento y calcula el promedio de edad de las personas en la matriz.
* La función `promedio_edad` utiliza un bucle `foreach` para sumar las edades de las personas en la matriz y luego divide la suma por el número de personas para obtener el promedio.
* Finalmente, se utiliza la función `promedio_edad` para calcular el promedio de edad de las personas en la matriz y se imprime el resultado.