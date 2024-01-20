```julia

# Función para calcular el factorial de un número natural.
function factorial(n::Integer)
    if n == 0
        return 1
    end

    return n * factorial(n-1)
end

# Función para calcular la combinación de n elementos tomados de r en r.
function combinacion(n::Integer, r::Integer)
    return factorial(n) / (factorial(r) * factorial(n-r))
end

# Función para calcular la permutación de n elementos tomados de r en r.
function permutacion(n::Integer, r::Integer)
    return factorial(n) / factorial(n-r)
end

# Función para calcular la probabilidad de un evento.
function probabilidad(p::Float64)
    return p
end

# Función para calcular la probabilidad condicional de un evento A dado que ha ocurrido el evento B.
function probabilidad_condicional(p_a::Float64, p_b::Float64, p_a_dado_b::Float64)
    return p_a_dado_b * p_b / p_a
end

# Función para calcular la probabilidad bayesiana de un evento A dado que ha ocurrido el evento B.
function probabilidad_bayesiana(p_a::Float64, p_b_dado_a::Float64)
    return p_b_dado_a * p_a / (p_b_dado_a * p_a + (1-p_b_dado_a) * (1-p_a))
end

```

Explicación:

* La función `factorial` calcula el factorial de un número natural.
* La función `combinacion` calcula la combinación de n elementos tomados de r en r.
* La función `permutacion` calcula la permutación de n elementos tomados de r en r.
* La función `probabilidad` calcula la probabilidad de un evento.
* La función `probabilidad_condicional` calcula la probabilidad condicional de un evento A dado que ha ocurrido el evento B.
* La función `probabilidad_bayesiana` calcula la probabilidad bayesiana de un evento A dado que ha ocurrido el evento B.