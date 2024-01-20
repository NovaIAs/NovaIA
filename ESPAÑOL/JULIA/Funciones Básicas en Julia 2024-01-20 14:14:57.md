```julia

# Definimos una función "calcular_area_triangulo" que calcula el área de un triángulo conociendo su base y su altura.
function calcular_area_triangulo(base::Real, altura::Real)
    return (base * altura) / 2
end

# Definimos una función recursiva "factorial" que calcula el factorial de un número.
function factorial(n::Int)
    if n == 0
        return 1
    else
        return n * factorial(n - 1)
    end
end

# Definimos una función "suma_lista" que suma todos los elementos de una lista.
function suma_lista(lista::Vector{Real})
    suma = 0
    for elemento in lista
        suma += elemento
    end
    return suma
end

# Definimos una función "filtrar_lista" que filtra los elementos de una lista que cumplen una cierta condición.
function filtrar_lista(lista::Vector{Real}, condicion::Function)
    lista_filtrada = []
    for elemento in lista
        if condicion(elemento)
            push!(lista_filtrada, elemento)
        end
    end
    return lista_filtrada
end

# Definimos una función "ordenar_lista" que ordena los elementos de una lista.
function ordenar_lista(lista::Vector{Real})
    ordenada = sort(lista)
    return ordenada
end

# Definimos una función "invertir_lista" que invierte el orden de los elementos de una lista.
function invertir_lista(lista::Vector{Real})
    invertida = reverse(lista)
    return invertida
end

```