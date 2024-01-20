```julia

# Definimos una función recursiva para calcular el factorial de un número.
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Definimos una función para calcular la suma de los dígitos de un número.
function suma_digitos(n)
    if n == 0
        return 0
    else
        return (n % 10) + suma_digitos(n / 10)
    end
end

# Definimos una función para comprobar si un número es primo.
function es_primo(n)
    if n <= 1
        return false
    else
        for i in 2:Int(sqrt(n))
            if n % i == 0
                return false
            end
        end
        return true
    end
end

# Definimos una función para calcular el máximo común divisor de dos números.
function mcd(a, b)
    if b == 0
        return a
    else
        return mcd(b, a % b)
    end
end

# Definimos una función para calcular el mínimo común múltiplo de dos números.
function mcm(a, b)
    return (a * b) / mcd(a, b)
end

# Definimos una función para calcular la serie de Fibonacci hasta un número determinado.
function fibonacci(n)
    if n <= 1
        return n
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

# Definimos una función para calcular la suma de los números pares de una lista.
function suma_pares(lista)
    if length(lista) == 0
        return 0
    else
        if lista[1] % 2 == 0
            return lista[1] + suma_pares(lista[2:end])
        else
            return suma_pares(lista[2:end])
        end
    end
end

# Definimos una función para calcular el producto de los números impares de una lista.
function producto_impares(lista)
    if length(lista) == 0
        return 1
    else
        if lista[1] % 2 == 1
            return lista[1] * producto_impares(lista[2:end])
        else
            return producto_impares(lista[2:end])
        end
    end
end

```

Este código contiene varias funciones matemáticas útiles, como la función factorial, la función suma_digitos, la función es_primo, la función mcd, la función mcm, la función fibonacci, la función suma_pares y la función producto_impares. Estas funciones son ampliamente utilizadas en programación y matemáticas, y pueden ser útiles para resolver una variedad de problemas.