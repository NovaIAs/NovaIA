```julia
# Define una función que recibe un vector de números y devuelve el promedio.
function promedio(v)
    suma = 0
    for x in v
        suma += x
    end
    return suma / length(v)
end

# Define una función que recibe una matriz y devuelve la diagonal principal.
function diagonal_principal(m)
    n = size(m, 1)
    d = Vector{Float64}(undef, n)
    for i in 1:n
        d[i] = m[i, i]
    end
    return d
end

# Define una función que recibe un vector de números y devuelve el máximo común divisor.
function mcd(v)
    m = v[1]
    for x in v[2:end]
        m = gcd(m, x)
    end
    return m
end

# Define una función que recibe un número y devuelve su factorial.
function factorial(n)
    if n < 0
        throw(ArgumentError("El número debe ser no negativo."))
    elseif n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Define una función que recibe un número y devuelve sus dígitos en orden inverso.
function reverse_digits(n)
    s = string(n)
    r = ""
    for i in reverse(1:length(s))
        r = r * s[i]
    end
    return parse(Int, r)
end

# Define una función que recibe una lista de enteros y devuelve la lista de sus cuadrados.
function cuadrados(l)
    return [x^2 for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus raíces cuadradas.
function raices_cuadradas(l)
    return [sqrt(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus senos.
function senos(l)
    return [sin(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus cosenos.
function cosenos(l)
    return [cos(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus tangentes.
function tangentes(l)
    return [tan(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus logaritmos naturales.
function logaritmos_naturales(l)
    return [log(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus logaritmos en base 10.
function logaritmos_en_base_10(l)
    return [log10(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus exponenciales.
function exponenciales(l)
    return [exp(x) for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus potencias de 2.
function potencias_de_2(l)
    return [2^x for x in l]
end

# Define una función que recibe una lista de números y devuelve la lista de sus potencias de 3.
function potencias_de_3(l)
    return [3^x for x in l]
end
```

Este código contiene una colección de funciones matemáticas que realizan diversas operaciones sobre vectores, matrices y listas de números. Las funciones incluyen:

* **promedio**: Calcula el promedio de un vector de números.
* **diagonal_principal**: Devuelve la diagonal principal de una matriz.
* **mcd**: Calcula el máximo común divisor de un vector de números.
* **factorial**: Calcula el factorial de un número.
* **reverse_digits**: Invierte los dígitos de un número.
* **cuadrados**: Calcula los cuadrados de una lista de números.
* **raices_cuadradas**: Calcula las raíces cuadradas de una lista de números.
* **senos**: Calcula los senos de una lista de números.
* **cosenos**: Calcula los cosenos de una lista de números.
* **tangentes**: Calcula las tangentes de una lista de números.
* **logaritmos_naturales**: Calcula los logaritmos naturales de una lista de números.
* **logaritmos_en_base_10**: Calcula los logaritmos en base 10 de una lista de números.
* **exponenciales**: Calcula los exponenciales de una lista de números.
* **potencias_de_2**: Calcula las potencias de 2 de una lista de números.
* **potencias_de_3**: Calcula las potencias de 3 de una lista de números.