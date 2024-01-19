```julia

# Definir una función recursiva para calcular el factorial de un número
factorial(n) = n == 0 ? 1 : n * factorial(n-1)

# Definir una función para calcular la suma de los cuadrados de los primeros n números naturales
sum_squares(n) = sum(i^2 for i in 1:n)

# Definir una función para calcular el producto de los primeros n números naturales
product_naturals(n) = foldl(*, 1, 1:n)

# Definir una función para generar una lista de los primeros n números naturales
natural_numbers(n) = collect(1:n)

# Definir una función para generar una lista de los primeros n números pares
even_numbers(n) = collect(filter(isodd, 1:n))

# Definir una función para generar una lista de los primeros n números impares
odd_numbers(n) = collect(filter(isodd, 1:n))

# Definir una función para generar una lista de los primeros n números primos
prime_numbers(n) = collect(filter(isprime, 1:n))

# Definir una función para generar una lista de las primeras n potencias de 2
powers_of_2(n) = collect(2 .^ (1:n))

# Definir una función para generar una lista de los primeros n números de Fibonacci
fibonacci_numbers(n) = collect(fib(1:n))

# Definir una función para calcular el valor máximo de una lista de números
max_value(list) = maximum(list)

# Definir una función para calcular el valor mínimo de una lista de números
min_value(list) = minimum(list)

# Definir una función para calcular el promedio de una lista de números
average_value(list) = sum(list) / length(list)

# Definir una función para calcular la varianza de una lista de números
variance(list) = sum((x - average_value(list))^2 for x in list) / (length(list) - 1)

# Definir una función para calcular la desviación estándar de una lista de números
standard_deviation(list) = sqrt(variance(list))

# Definir una función para calcular la mediana de una lista de números
median_value(list) = sort(list)[div(length(list), 2) + 1]

# Definir una función para calcular el modo de una lista de números
mode_value(list) = findmode(list)[1]

# Definir una función para generar una lista de los elementos únicos de una lista de números
unique_values(list) = unique(list)

# Definir una función para contar el número de apariciones de un elemento en una lista de números
count_value(list, element) = count(list, element)

# Definir una función para generar una lista de los elementos que aparecen más de una vez en una lista de números
duplicate_values(list) = filter(x -> count_value(list, x) > 1, list)

# Definir una función para generar una lista de los elementos que aparecen solo una vez en una lista de números
unique_values(list) = filter(x -> count_value(list, x) == 1, list)

# Definir una función para generar una lista de los elementos que aparecen en ambas listas
intersection(list1, list2) = intersect(list1, list2)

# Definir una función para generar una lista de los elementos que aparecen en una lista pero no en la otra
set_difference(list1, list2) = setdiff(list1, list2)

# Definir una función para generar una lista de los elementos que aparecen en cualquiera de las dos listas
set_union(list1, list2) = union(list1, list2)

# Definir una función para generar una lista de los elementos que aparecen en una lista pero no en la otra
symmetric_difference(list1, list2) = symdiff(list1, list2)

```