```lua
-- Este código implementa un algoritmo de clasificación rápida en Lua.

-- La función `quicksort` recibe una lista de elementos y la ordena de menor a mayor.
-- Utiliza el algoritmo de clasificación rápida, que es un algoritmo recursivo.
function quicksort(lista)
    if #lista <= 1 then
        return lista
    end

    -- Elegir un elemento pivote.
    pivote = lista[math.random(#lista)]

    -- Dividir la lista en dos partes: los elementos menores que el pivote y los mayores que el pivote.
    menores = {}
    mayores = {}
    for i = 1, #lista do
        if lista[i] < pivote then
            menores[#menores+1] = lista[i]
        else
            mayores[#mayores+1] = lista[i]
        end
    end

    -- Ordenar recursivamente las dos partes.
    menores = quicksort(menores)
    mayores = quicksort(mayores)

    -- Concatenar las dos partes ordenadas y el pivote para obtener la lista ordenada final.
    return menores .. {pivote} .. mayores
end

-- Ejemplo de uso:
lista = {5, 3, 1, 2, 4}
print("Lista original:", lista)
lista_ordenada = quicksort(lista)
print("Lista ordenada:", lista_ordenada)
```

Explicación del código:

* La función `quicksort` recibe una lista de elementos y la ordena de menor a mayor utilizando el algoritmo de clasificación rápida.
* El algoritmo de clasificación rápida funciona seleccionando un elemento pivote de la lista y dividiendo la lista en dos partes: los elementos menores que el pivote y los mayores que el pivote.
* A continuación, ordena recursivamente las dos partes y concatena las dos partes ordenadas y el pivote para obtener la lista ordenada final.
* En el ejemplo de uso, se ordena una lista de números de menor a mayor y se imprime la lista original y la lista ordenada.