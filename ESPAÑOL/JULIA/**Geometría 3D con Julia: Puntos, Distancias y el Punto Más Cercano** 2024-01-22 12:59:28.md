```julia
# Crear un tipo de datos personalizado
type Punto3D
    x::Float64
    y::Float64
    z::Float64
end

# Definir una función para crear un punto 3D
function crearPunto3D(x, y, z)
    Punto3D(x, y, z)
end

# Crear una lista de puntos 3D
puntos3D = [crearPunto3D(1.0, 2.0, 3.0), crearPunto3D(4.0, 5.0, 6.0), crearPunto3D(7.0, 8.0, 9.0)]

# Definir una función para calcular la distancia entre dos puntos 3D
function distancia3D(punto1, punto2)
    sqrt((punto1.x - punto2.x)^2 + (punto1.y - punto2.y)^2 + (punto1.z - punto2.z)^2)
end

# Calcular la distancia entre cada par de puntos 3D en la lista
for i in 1:length(puntos3D)
    for j in i+1:length(puntos3D)
        println("La distancia entre el punto $puntos3D[i] y el punto $puntos3D[j] es $(distancia3D(puntos3D[i], puntos3D[j]))")
    end
end

# Definir una función para encontrar el punto 3D más cercano a un punto dado
function encontrarPunto3DCercano(punto, puntos3D)
    distanciaMinima = Inf
    puntoCercano = Punto3D(0.0, 0.0, 0.0)

    for punto3D in puntos3D
        distancia = distancia3D(punto, punto3D)
        if distancia < distanciaMinima
            distanciaMinima = distancia
            puntoCercano = punto3D
        end
    end

    return puntoCercano
end

# Encontrar el punto 3D más cercano al punto (1.0, 2.0, 3.0)
puntoCercano = encontrarPunto3DCercano(crearPunto3D(1.0, 2.0, 3.0), puntos3D)

# Imprimir el punto 3D más cercano
println("El punto 3D más cercano al punto (1.0, 2.0, 3.0) es $puntoCercano")
```

Este código crea un tipo de datos personalizado llamado `Punto3D` que representa un punto en el espacio 3D. 
A continuación, define una función para crear un punto 3D y una lista de puntos 3D. 
Luego, define una función para calcular la distancia entre dos puntos 3D y calcula la distancia entre cada par de puntos 3D en la lista. 
A continuación, define una función para encontrar el punto 3D más cercano a un punto dado y encuentra el punto 3D más cercano al punto (1.0, 2.0, 3.0). 
Por último, imprime el punto 3D más cercano.