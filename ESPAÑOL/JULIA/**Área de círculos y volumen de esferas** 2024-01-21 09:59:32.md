```julia
# Definir una función que calcule el área de un círculo
function area_circulo(radio)
    π = 3.141592653589793
    return π * radio^2
end

# Crear un vector de radios
radios = [1.0, 2.0, 3.0, 4.0, 5.0]

# Usar la función `map` para calcular el área de cada círculo
areas = map(area_circulo, radios)

# Imprimir los resultados
println("Las áreas de los círculos son:")
for area in areas
    println(area)
end

# Crear una función que calcule el volumen de una esfera
function volumen_esfera(radio)
    π = 3.141592653589793
    return (4/3) * π * radio^3
end

# Usar la función `map` para calcular el volumen de cada esfera
volumenes = map(volumen_esfera, radios)

# Imprimir los resultados
println("Los volúmenes de las esferas son:")
for volumen in volumenes
    println(volumen)
end
```

Este código calcula el área de un círculo y el volumen de una esfera para una serie de radios dados. Utiliza la función `map` de Julia para aplicar la función `area_circulo` y la función `volumen_esfera` a cada uno de los elementos del vector `radios`. Los resultados se imprimen en la consola.