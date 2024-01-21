```julia
using LinearAlgebra, Plots

# Definimos una función para calcular el valor esperado de una variable aleatoria discreta.

function valor_esperado(X, pmf)
    E(X) = sum(X .* pmf)
    return E(X)
end

# Definimos una función para generar una muestra aleatoria de una variable aleatoria discreta.

function generar_muestra(X, pmf)
    n = length(X)
    u = rand(n)
    muestra = zeros(n)
    for i in 1:n
        for j in 1:n
            if u[i] < pmf[j]
                muestra[i] = X[j]
                break
            end
        end
    end
    return muestra
end

# Definimos una función para estimar el valor esperado de una variable aleatoria discreta a partir de una muestra.

function estimar_valor_esperado(muestra)
    n = length(muestra)
    E_muestra = sum(muestra) / n
    return E_muestra
end

# Definimos una función para calcular el error cuadrático medio de una estimación del valor esperado.

function error_cuadratico_medio(E_real, E_estimado)
    ECM = (E_real - E_estimado)^2
    return ECM
end

# Definimos los parámetros de la variable aleatoria discreta.

X = [1, 2, 3, 4, 5]
pmf = [0.2, 0.3, 0.2, 0.1, 0.2]

# Calculamos el valor esperado real de la variable aleatoria discreta.

E_real = valor_esperado(X, pmf)

# Generamos una muestra aleatoria de la variable aleatoria discreta.

muestra = generar_muestra(X, pmf)

# Estimamos el valor esperado de la variable aleatoria discreta a partir de la muestra.

E_estimado = estimar_valor_esperado(muestra)

# Calculamos el error cuadrático medio de la estimación del valor esperado.

ECM = error_cuadratico_medio(E_real, E_estimado)

# Imprimimos los resultados.

println("Valor esperado real: $E_real")
println("Valor esperado estimado: $E_estimado")
println("Error cuadrático medio: $ECM")

# Creamos un histograma de la muestra.

histogram(muestra, title="Histograma de la muestra")
```

Este código simula el proceso de generar una muestra aleatoria de una variable aleatoria discreta, estimar el valor esperado de la variable aleatoria discreta a partir de la muestra y calcular el error cuadrático medio de la estimación del valor esperado. El código también crea un histograma de la muestra.