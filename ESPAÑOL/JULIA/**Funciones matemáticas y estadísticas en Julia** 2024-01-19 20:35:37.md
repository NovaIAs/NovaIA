```julia

# Definición de función para calcular el factorial de un número
factorial(n) = reduce(*, 1:n)

# Definición de función para calcular el máximo común divisor de dos números
mcd(a, b) = @assert a > 0 && b > 0 begin
    while b != 0
        t = a % b
        a = b
        b = t
    end
    return a
end

# Definición de función para calcular el mínimo común múltiplo de dos números
mcm(a, b) = div(a * b, mcd(a, b))

# Definición de función para comprobar si un número es primo
isprime(n) = @assert n > 1 begin
    for p in 2:floor(sqrt(n))
        if n % p == 0
            return false
        end
    end
    return true
end

# Definición de función para encontrar todos los números primos hasta un número determinado
primes(n) = @assert n > 0 begin
    p = Vector{Int64}()
    for i in 2:n
        if isprime(i)
            push!(p, i)
        end
    end
    return p
end

# Definición de función para generar una lista de números aleatorios
randomlist(n, min, max) = @assert n > 0 && min < max begin
    r = Vector{Int64}(undef, n)
    for i in 1:n
        r[i] = rand(min, max)
    end
    return r
end

# Definición de función para calcular la media de una lista de números
mean(v) = @assert length(v) > 0 begin
    sum = 0.0
    for x in v
        sum += x
    end
    return sum / length(v)
end

# Definición de función para calcular la desviación estándar de una lista de números
std(v) = @assert length(v) > 1 begin
    m = mean(v)
    sum = 0.0
    for x in v
        sum += (x - m)^2
    end
    return sqrt(sum / (length(v) - 1))
end

# Definición de función para calcular la correlación entre dos listas de números
corr(x, y) = @assert length(x) == length(y) begin
    m_x = mean(x)
    m_y = mean(y)
    sum = 0.0
    for i in 1:length(x)
        sum += (x[i] - m_x) * (y[i] - m_y)
    end
    return sum / (std(x) * std(y) * (length(x) - 1))
end

# Definición de función para ajustar una línea recta a una lista de puntos
fitline(x, y) = @assert length(x) == length(y) begin
    m = sum((x.-mean(x)).*(y.-mean(y))) / sum((x.-mean(x)).^2)
    b = mean(y) - m * mean(x)
    return (m, b)
end

# Definición de función para dibujar una gráfica
plotgraph(x, y, title, xlabel, ylabel) = @assert length(x) == length(y) begin
    using Plots
    pyplot()
    scatter(x, y, markersize=10, label="Datos")
    xlabel(xlabel)
    ylabel(ylabel)
    title(title)
    legend()
end

```

Este código Julia contiene un conjunto de funciones útiles para realizar cálculos matemáticos y estadísticos.

- La función `factorial` calcula el factorial de un número.
- La función `mcd` calcula el máximo común divisor de dos números.
- La función `mcm` calcula el mínimo común múltiplo de dos números.
- La función `isprime` comprueba si un número es primo.
- La función `primes` encuentra todos los números primos hasta un número determinado.
- La función `randomlist` genera una lista de números aleatorios.
- La función `mean` calcula la media de una lista de números.
- La función `std` calcula la desviación estándar de una lista de números.
- La función `corr` calcula la correlación entre dos listas de números.
- La función `fitline` ajusta una línea recta a una lista de puntos.
- La función `plotgraph` dibuja una gráfica.

Este código puede ser utilizado para realizar una variedad de tareas matemáticas y estadísticas, como calcular el factorial de un número, encontrar los números primos hasta un número determinado, generar una lista de números aleatorios, calcular la media y la desviación estándar de una lista de números, calcular la correlación entre dos listas de números, ajustar una línea recta a una lista de puntos y dibujar una gráfica.