```julia
# Definir una función recursiva para calcular el factorial de un número
factorial(n) = n == 0 ? 1 : n * factorial(n-1)

# Definir una función para generar una lista de números primos hasta un número dado
primos(n) = [i for i in 1:n if isprime(i)]

# Definir una función para comprobar si un número es primo
isprime(n) = n > 1 && (all(n % i != 0 for i in 2:floor(sqrt(n))))

# Definir una función para generar una lista de los dígitos de un número
dígitos(n) = reverse(map(Int, string(n)))

# Definir una función para comprobar si un número es capicúa
capicúa(n) = n == reverse(n)

# Generar una lista de los números primos hasta 1000
primos1000 = primos(1000)

# Filtrar los números primos que son capicúas
primos_capicúas = filter(capicúa, primos1000)

# Imprimir los números primos capicúas
println("Números primos capicúas hasta 1000:")
for p in primos_capicúas
    println(p)
end

# Generar una lista de los dígitos de cada número primo capicúa
dígitos_primos_capicúas = map(dígitos, primos_capicúas)

# Calcular el factorial de cada lista de dígitos
factoriales_dígitos_primos_capicúas = map(factorial, dígitos_primos_capicúas)

# Sumar los factoriales de las listas de dígitos
suma_factoriales_dígitos_primos_capicúas = sum(factoriales_dígitos_primos_capicúas)

# Imprimir la suma de los factoriales de las listas de dígitos
println("Suma de los factoriales de los dígitos de los números primos capicúas hasta 1000:")
println(suma_factoriales_dígitos_primos_capicúas)
```

Explicación:

1. Definimos una función recursiva `factorial` para calcular el factorial de un número.
2. Definimos una función `primos` para generar una lista de números primos hasta un número dado.
3. Definimos una función `isprime` para comprobar si un número es primo.
4. Definimos una función `dígitos` para generar una lista de los dígitos de un número.
5. Definimos una función `capicúa` para comprobar si un número es capicúa.
6. Generamos una lista de los números primos hasta 1000 utilizando la función `primos`.
7. Filtramos los números primos que son capicúas utilizando la función `filter`.
8. Imprimimos los números primos capicúas.
9. Generamos una lista de los dígitos de cada número primo capicúa utilizando la función `map`.
10. Calculamos el factorial de cada lista de dígitos utilizando la función `factorial`.
11. Sumamos los factoriales de las listas de dígitos.
12. Imprimimos la suma de los factoriales de las listas de dígitos.