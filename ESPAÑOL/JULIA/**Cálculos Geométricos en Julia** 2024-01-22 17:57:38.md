```julia

# Función para calcular el área de un triángulo usando la fórmula de Herón
function area_triangulo(a, b, c)
    s = (a + b + c) / 2
    return sqrt(s * (s - a) * (s - b) * (s - c))
end

# Función para calcular el volumen de un cubo usando la fórmula V = a^3
function volumen_cubo(a)
    return a^3
end

# Función para calcular el perímetro de un cuadrado usando la fórmula P = 4a
function perimetro_cuadrado(a)
    return 4 * a
end

# Función para calcular la hipotenusa de un triángulo rectángulo usando la fórmula c^2 = a^2 + b^2
function hipotenusa(a, b)
    return sqrt(a^2 + b^2)
end

# Función para calcular la distancia entre dos puntos en un plano cartesiano usando la fórmula d = sqrt((x1 - x2)^2 + (y1 - y2)^2)
function distancia(x1, y1, x2, y2)
    return sqrt((x1 - x2)^2 + (y1 - y2)^2)
end

# Función para calcular el área de un círculo usando la fórmula A = πr^2
function area_circulo(r)
    return π * r^2
end

# Función para calcular el perímetro de un círculo usando la fórmula P = 2πr
function perimetro_circulo(r)
    return 2 * π * r
end

# Función para calcular el volumen de una esfera usando la fórmula V = (4/3)πr^3
function volumen_esfera(r)
    return (4/3) * π * r^3
end

# Función para calcular el área de una esfera usando la fórmula A = 4πr^2
function area_esfera(r)
    return 4 * π * r^2
end

```

Explicación del código:

* La primera línea define una función llamada `area_triangulo` que calcula el área de un triángulo dado sus tres lados. La función utiliza la fórmula de Herón para calcular el área.
* La segunda línea define una función llamada `volumen_cubo` que calcula el volumen de un cubo dado su lado. La función utiliza la fórmula `V = a^3` para calcular el volumen.
* La tercera línea define una función llamada `perimetro_cuadrado` que calcula el perímetro de un cuadrado dado su lado. La función utiliza la fórmula `P = 4a` para calcular el perímetro.
* La cuarta línea define una función llamada `hipotenusa` que calcula la hipotenusa de un triángulo rectángulo dados sus dos catetos. La función utiliza la fórmula `c^2 = a^2 + b^2` para calcular la hipotenusa.
* La quinta línea define una función llamada `distancia` que calcula la distancia entre dos puntos en un plano cartesiano dados sus coordenadas. La función utiliza la fórmula `d = sqrt((x1 - x2)^2 + (y1 - y2)^2)` para calcular la distancia.
* La sexta línea define una función llamada `area_circulo` que calcula el área de un círculo dado su radio. La función utiliza la fórmula `A = πr^2` para calcular el área.
* La séptima línea define una función llamada `perimetro_circulo` que calcula el perímetro de un círculo dado su radio. La función utiliza la fórmula `P = 2πr` para calcular el perímetro.
* La octava línea define una función llamada `volumen_esfera` que calcula el volumen de una esfera dado su radio. La función utiliza la fórmula `V = (4/3)πr^3` para calcular el volumen.
* La novena línea define una función llamada `area_esfera` que calcula el área de una esfera dado su radio. La función utiliza la fórmula `A = 4πr^2` para calcular el área.