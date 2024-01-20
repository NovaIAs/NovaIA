```lua
-- Definición de funciones

-- Función para calcular el factorial de un número
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Función para calcular el máximo común divisor de dos números
function mcd(a, b)
    if b == 0 then
        return a
    else
        return mcd(b, a % b)
    end
end

-- Función para calcular el mínimo común múltiplo de dos números
function mcm(a, b)
    return (a * b) / mcd(a, b)
end

-- Función para generar una lista de números aleatorios
function generarListaAleatoria(n)
    local lista = {}

    for i = 1, n do
        lista[i] = math.random(1, 100)
    end

    return lista
end

-- Función para encontrar el elemento máximo de una lista
function elementoMaximo(lista)
    local maximo = lista[1]

    for i = 2, #lista do
        if lista[i] > maximo then
            maximo = lista[i]
        end
    end

    return maximo
end

-- Función para encontrar el elemento mínimo de una lista
function elementoMinimo(lista)
    local minimo = lista[1]

    for i = 2, #lista do
        if lista[i] < minimo then
            minimo = lista[i]
        end
    end

    return minimo
end

-- Función para ordenar una lista de números en orden ascendente
function ordenarListaAsc(lista)
    local listaOrdenada = {}

    while #lista > 0 do
        local minimo = elementoMinimo(lista)
        listaOrdenada[#listaOrdenada + 1] = minimo
        lista = table.remove(lista, table.find(lista, minimo))
    end

    return listaOrdenada
end

-- Función para ordenar una lista de números en orden descendente
function ordenarListaDesc(lista)
    local listaOrdenada = {}

    while #lista > 0 do
        local maximo = elementoMaximo(lista)
        listaOrdenada[#listaOrdenada + 1] = maximo
        lista = table.remove(lista, table.find(lista, maximo))
    end

    return listaOrdenada
end

-- Función para imprimir una tabla
function imprimirTabla(tabla)
    for i, v in pairs(tabla) do
        print(i .. " -> " .. v)
    end
end

-- Función para imprimir una lista
function imprimirLista(lista)
    for i = 1, #lista do
        print(lista[i])
    end
end

-- Uso de las funciones

-- Calcular el factorial de un número
print("Factorial de 5: " .. factorial(5))

-- Calcular el máximo común divisor de dos números
print("Máximo común divisor de 12 y 18: " .. mcd(12, 18))

-- Calcular el mínimo común múltiplo de dos números
print("Mínimo común múltiplo de 12 y 18: " .. mcm(12, 18))

-- Generar una lista de números aleatorios
local listaAleatoria = generarListaAleatoria(10)

-- Imprimir la lista de números aleatorios
print("Lista de números aleatorios:")
imprimirLista(listaAleatoria)

-- Encontrar el elemento máximo de una lista
print("Elemento máximo de la lista: " .. elementoMaximo(listaAleatoria))

-- Encontrar el elemento mínimo de una lista
print("Elemento mínimo de la lista: " .. elementoMinimo(listaAleatoria))

-- Ordenar la lista de números en orden ascendente
print("Lista ordenada en orden ascendente:")
imprimirLista(ordenarListaAsc(listaAleatoria))

-- Ordenar la lista de números en orden descendente
print("Lista ordenada en orden descendente:")
imprimirLista(ordenarListaDesc(listaAleatoria))

-- Imprimir una tabla
local tabla = {
    ["nombre"] = "Juan",
    ["apellido"] = "García",
    ["edad"] = 25
}

print("Tabla:")
imprimirTabla(tabla)
```

Explicación del código:

* Se definen una serie de funciones para realizar cálculos matemáticos y operaciones con listas.
* Se genera una lista de números aleatorios.
* Se imprimen la lista de números aleatorios, el elemento máximo, el elemento mínimo y la lista ordenada en orden ascendente y descendente.
* Se imprime una tabla con datos personales.