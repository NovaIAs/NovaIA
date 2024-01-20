```julia

# Definición de una función que calcula el factorial de un número
function factorial(n::Int)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Definición de una función que calcula la suma de una lista de números
function sum(list::Vector{Int})
    total = 0
    for x in list
        total += x
    end
    return total
end

# Definición de una función que calcula el producto de una lista de números
function product(list::Vector{Int})
    total = 1
    for x in list
        total *= x
    end
    return total
end

# Definición de una función que calcula el promedio de una lista de números
function mean(list::Vector{Int})
    total = sum(list)
    return total / length(list)
end

# Definición de una función que calcula la mediana de una lista de números
function median(list::Vector{Int})
    sorted_list = sort(list)
    length_list = length(sorted_list)
    if length_list % 2 == 0
        median_index = length_list / 2
        return (sorted_list[median_index] + sorted_list[median_index + 1]) / 2
    else
        median_index = (length_list + 1) / 2
        return sorted_list[median_index]
    end
end

# Definición de una función que calcula la desviación estándar de una lista de números
function std(list::Vector{Int})
    mean_value = mean(list)
    sum_squared_differences = 0
    for x in list
        sum_squared_differences += (x - mean_value)^2
    end
    variance = sum_squared_differences / (length(list) - 1)
    return sqrt(variance)
end

# Definición de una función que calcula la correlación entre dos listas de números
function correlation(list1::Vector{Int}, list2::Vector{Int})
    mean1 = mean(list1)
    mean2 = mean(list2)
    cov = 0
    for i in 1:length(list1)
        cov += (list1[i] - mean1) * (list2[i] - mean2)
    end
    var1 = std(list1)^2
    var2 = std(list2)^2
    return cov / sqrt(var1 * var2)
end

# Definición de una función que calcula la regresión lineal entre dos listas de números
function linear_regression(list1::Vector{Int}, list2::Vector{Int})
    mean1 = mean(list1)
    mean2 = mean(list2)
    cov = 0
    for i in 1:length(list1)
        cov += (list1[i] - mean1) * (list2[i] - mean2)
    end
    var1 = std(list1)^2
    slope = cov / var1
    intercept = mean2 - slope * mean1
    return slope, intercept
end

```