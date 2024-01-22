```julia

# Definir una función para calcular el factorial de un número
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n - 1)
    end
end

# Definir una función para calcular la suma de los dígitos de un número
function sum_digits(n)
    if n < 10
        return n
    else
        return n % 10 + sum_digits(n // 10)
    end
end

# Definir una función para comprobar si un número es primo
function is_prime(n)
    if n < 2
        return false
    end
    for i in 2:sqrt(Int, n)
        if n % i == 0
            return false
        end
    end
    return true
end

# Definir una función para encontrar el mayor divisor común de dos números
function gcd(a, b)
    if b == 0
        return a
    else
        return gcd(b, a % b)
    end
end

# Definir una función para encontrar el menor múltiplo común de dos números
function lcm(a, b)
    return a * b / gcd(a, b)
end

# Definir una función para encontrar el número de dígitos de un número
function num_digits(n)
    if n < 10
        return 1
    else
        return 1 + num_digits(n // 10)
    end
end

# Definir una función para convertir un número a una cadena de caracteres
function num_to_str(n)
    if n < 10
        return string(n)
    else
        return num_to_str(n // 10) * string(n % 10)
    end
end

# Definir una función para convertir una cadena de caracteres a un número
function str_to_num(s)
    if s == ""
        return 0
    else
        return (s[1] - '0') * 10^(s[2] - '0') + str_to_num(s[3:end])
    end
end

# Definir una función para encontrar el número de elementos de una lista
function length(list)
    if list == []
        return 0
    else
        return 1 + length(list[2:end])
    end
end

# Definir una función para encontrar el índice de un elemento en una lista
function index_of(element, list)
    for i in 1:length(list)
        if list[i] == element
            return i
        end
    end
    return -1
end

# Definir una función para insertar un elemento en una lista en una posición determinada
function insert_at(element, list, index)
    if index == 1
        return [element, list]
    else
        return [list[1:index-1], element, list[index:end]]
    end
end

# Definir una función para eliminar un elemento de una lista en una posición determinada
function delete_at(list, index)
    if index == 1
        return list[2:end]
    else
        return [list[1:index-1], list[index+1:end]]
    end
end

# Definir una función para encontrar el elemento máximo de una lista
function max(list)
    if length(list) == 1
        return list[1]
    end
    if list[1] > max(list[2:end])
        return list[1]
    else
        return max(list[2:end])
    end
end

# Definir una función para encontrar el elemento mínimo de una lista
function min(list)
    if length(list) == 1
        return list[1]
    end
    if list[1] < min(list[2:end])
        return list[1]
    else
        return min(list[2:end])
    end
end

# Definir una función para ordenar una lista en orden ascendente
function sort(list)
    if length(list) <= 1
        return list
    end
    pivot = list[1]
    left = []
    right = []
    for i in 2:length(list)
        if list[i] < pivot
            left = insert_at(list[i], left, length(left) + 1)
        else
            right = insert_at(list[i], right, length(right) + 1)
        end
    end
    return sort(left) * [pivot] * sort(right)
end

# Definir una función para ordenar una lista en orden descendente
function sort_desc(list)
    return sort(list)[length(list):-1:1]
end

# Definir una función para encontrar el elemento más frecuente en una lista
function mode(list)
    frequency = Dict()
    for element in list
        if frequency.haskey(element)
            frequency[element] += 1
        else
            frequency[element] = 1
        end
    end
    max_frequency = 0
    max_element = list[1]
    for element in frequency
        if frequency[element] > max_frequency
            max_frequency = frequency[element]
            max_element = element
        end
    end
    return max_element
end

# Definir una función para encontrar la mediana de una lista
function median(list)
    list = sort(list)
    if length(list) % 2 == 0
        return (list[length(list) / 2] + list[length(list) / 2 + 1]) / 2
    else
        return list[length(list) / 2 + 1]
    end
end

# Definir una función para encontrar la media de una lista
function mean(list)
    sum = 0
    for element in list
        sum += element
    end
    return sum / length(list)
end

```