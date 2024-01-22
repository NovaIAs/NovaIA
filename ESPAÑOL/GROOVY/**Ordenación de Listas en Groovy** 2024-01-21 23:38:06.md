```groovy
// Definición de lista de números
def numeros = [10, 20, 30, 40, 50]

// Definición de una función de ordenación ascendente
def ordenarAscendente = { a, b -> a <=> b }

// Definición de una función de ordenación descendente
def ordenarDescendente = { a, b -> b <=> a }

// Ordenación de la lista de números en orden ascendente
def numerosOrdenadosAsc = numeros.sort(ordenarAscendente)

// Ordenación de la lista de números en orden descendente
def numerosOrdenadosDesc = numeros.sort(ordenarDescendente)

// Impresión de las listas ordenadas
println "Lista de números ordenada en orden ascendente: ${numerosOrdenadosAsc}"
println "Lista de números ordenada en orden descendente: ${numerosOrdenadosDesc}"

// Definición de una lista de nombres
def nombres = ["Juan", "María", "Pedro", "Ana", "José"]

// Definición de una función de ordenación alfabética ascendente
def ordenarAlfabeticamenteAsc = { a, b -> a.compareToIgnoreCase(b) }

// Definición de una función de ordenación alfabética descendente
def ordenarAlfabeticamenteDesc = { a, b -> b.compareToIgnoreCase(a) }

// Ordenación de la lista de nombres en orden alfabético ascendente
def nombresOrdenadosAsc = nombres.sort(ordenarAlfabeticamenteAsc)

// Ordenación de la lista de nombres en orden alfabético descendente
def nombresOrdenadosDesc = nombres.sort(ordenarAlfabeticamenteDesc)

// Impresión de las listas ordenadas
println "Lista de nombres ordenada alfabéticamente en orden ascendente: ${nombresOrdenadosAsc}"
println "Lista de nombres ordenada alfabéticamente en orden descendente: ${nombresOrdenadosDesc}"

// Definición de una lista de artículos
def articulos = [
    [nombre: "Manzana", precio: 1.25],
    [nombre: "Naranja", precio: 1.50],
    [nombre: "Plátano", precio: 0.99],
    [nombre: "Uva", precio: 2.25],
    [nombre: "Cereza", precio: 3.00]
]

// Definición de una función de ordenación por precio ascendente
def ordenarPorPrecioAsc = { a, b -> a.precio <=> b.precio }

// Definición de una función de ordenación por precio descendente
def ordenarPorPrecioDesc = { a, b -> b.precio <=> a.precio }

// Ordenación de la lista de artículos por precio en orden ascendente
def articulosOrdenadosPrecioAsc = articulos.sort(ordenarPorPrecioAsc)

// Ordenación de la lista de artículos por precio en orden descendente
def articulosOrdenadosPrecioDesc = articulos.sort(ordenarPorPrecioDesc)

// Impresión de las listas ordenadas
println "Lista de artículos ordenada por precio en orden ascendente: ${articulosOrdenadosPrecioAsc}"
println "Lista de artículos ordenada por precio en orden descendente: ${articulosOrdenadosPrecioDesc}"

// Definición de una lista de personas
def personas = [
    [nombre: "Juan", edad: 25, ciudad: "Barcelona"],
    [nombre: "María", edad: 30, ciudad: "Madrid"],
    [nombre: "Pedro", edad: 28, ciudad: "Valencia"],
    [nombre: "Ana", edad: 22, ciudad: "Sevilla"],
    [nombre: "José", edad: 32, ciudad: "Bilbao"]
]

// Definición de una función de ordenación por edad ascendente
def ordenarPorEdadAsc = { a, b -> a.edad <=> b.edad }

// Definición de una función de ordenación por edad descendente
def ordenarPorEdadDesc = { a, b -> b.edad <=> a.edad }

// Ordenación de la lista de personas por edad en orden ascendente
def personasOrdenadasEdadAsc = personas.sort(ordenarPorEdadAsc)

// Ordenación de la lista de personas por edad en orden descendente
def personasOrdenadasEdadDesc = personas.sort(ordenarPorEdadDesc)

// Impresión de las listas ordenadas
println "Lista de personas ordenada por edad en orden ascendente: ${personasOrdenadasEdadAsc}"
println "Lista de personas ordenada por edad en orden descendente: ${personasOrdenadasEdadDesc}"

// Definición de una función de ordenación por ciudad ascendente
def ordenarPorCiudadAsc = { a, b -> a.ciudad.compareToIgnoreCase(b.ciudad) }

// Definición de una función de ordenación por ciudad descendente
