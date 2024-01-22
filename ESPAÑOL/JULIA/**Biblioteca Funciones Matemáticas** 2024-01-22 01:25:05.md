```julia
# Esta función calcula la factorial de un número.
function factorial(n)
    if n <= 1
        return 1
    end
    return n * factorial(n-1)
end

# Esta función calcula el número de combinaciones posibles de n elementos tomados de k en k.
function combinaciones(n, k)
    return factorial(n) / (factorial(k) * factorial(n-k))
end

# Esta función calcula el número de permutaciones posibles de n elementos tomados de k en k.
function permutaciones(n, k)
    return factorial(n) / factorial(n-k)
end

# Esta función calcula el número de variaciones posibles de n elementos tomados de k en k.
function variaciones(n, k)
    return factorial(n) / factorial(n-k)
end

# Esta función calcula el máximo común divisor de dos números.
function mcd(a, b)
    if b == 0
        return a
    end
    return mcd(b, a % b)
end

# Esta función calcula el mínimo común múltiplo de dos números.
function mcm(a, b)
    return a * b / mcd(a, b)
end

# Esta función calcula el valor absoluto de un número.
function abs(x)
    if x < 0
        return -x
    end
    return x
end

# Esta función calcula el signo de un número.
function sign(x)
    if x == 0
        return 0
    elseif x > 0
        return 1
    else
        return -1
    end
end

# Esta función calcula la raíz cuadrada de un número.
function sqrt(x)
    if x < 0
        return NaN
    end
    return x^(1/2)
end

# Esta función calcula el logaritmo natural de un número.
function log(x)
    if x <= 0
        return NaN
    end
    return log10(x) / log10(e)
end

# Esta función calcula el logaritmo en base 10 de un número.
function log10(x)
    if x <= 0
        return NaN
    end
    return log(x) / log(10)
end

# Esta función calcula el seno de un ángulo.
function sin(x)
    return cos(x - pi/2)
end

# Esta función calcula el coseno de un ángulo.
function cos(x)
    return exp(I*x).real
end

# Esta función calcula la tangente de un ángulo.
function tan(x)
    return sin(x) / cos(x)
end

# Esta función calcula la arcotangente de un número.
function atan(x)
    return atan2(x, 1)
end

# Esta función calcula la arcocotangente de un número.
function acot(x)
    return atan2(1, x)
end

# Esta función calcula la arcosecante de un número.
function asec(x