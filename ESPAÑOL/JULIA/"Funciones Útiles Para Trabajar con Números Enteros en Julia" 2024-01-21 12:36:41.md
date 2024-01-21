```julia
# Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(a, b)
    while b != 0
        t = b
        b = a % b
        a = t
    end
    return a
end

# Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(a, b)
    mcd_ab = mcd(a, b)
    return a * b / mcd_ab
end

# Función para encontrar los factores primos de un número
function factores_primos(n)
    factores = Vector{Int64}()
    i = 2
    while n != 1
        while n % i == 0
            push!(factores, i)
            n /= i
        end
        i += 1
    end
    return factores
end

# Función para encontrar todos los divisores de un número
function divisores(n)
    divisores = Vector{Int64}()
    for i in 1:n
        if n % i == 0
            push!(divisores, i)
        end
    end
    return divisores
end

# Función para encontrar el mayor factor primo de un número
function mayor_factor_primo(n)
    mayor_factor = 1
    i = 2
    while n != 1
        while n % i == 0
            mayor_factor = i
            n /= i
        end
        i += 1
    end
    return mayor_factor
end

# Función para encontrar el número de divisores de un número
function num_divisores(n)
    factores = factores_primos(n)
    exp = Vector{Int}()
    for f in factores
        exp.push!(count(factores, f))
    end
    num_divisores = 1
    for e in exp
        num_divisores *= e + 1
    end
    return num_divisores
end

# Función para encontrar la suma de los divisores de un número
function suma_divisores(n)
    factores = factores_primos(n)
    exp = Vector{Int}()
    for f in factores
        exp.push!(count(factores, f))
    end
    suma_divisores = 1
    for f, e in zip(factores, exp)
        suma_divisores *= (f^(e + 1) - 1) / (f - 1)
    end
    return suma_divisores
end
```

Este código define varias funciones útiles para trabajar con números enteros en Julia. Las funciones incluyen:

* `mcd`: calcula el máximo común divisor de dos números.
* `mcm`: calcula el mínimo común múltiplo de dos números.
* `factores_primos`: encuentra todos los factores primos de un número.
* `divisores`: encuentra todos los divisores de un número.
* `mayor_factor_primo`: encuentra el mayor factor primo de un número.
* `num_divisores`: calcula el número de divisores de un número.
* `suma_divisores`: calcula la suma de los divisores de un número.

Estas funciones se pueden utilizar para resolver una variedad de problemas matemáticos, como encontrar el número de divisores de un número o la suma de los divisores de un número.