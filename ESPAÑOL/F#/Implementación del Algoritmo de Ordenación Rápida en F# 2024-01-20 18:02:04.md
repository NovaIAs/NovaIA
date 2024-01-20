// Implementación de un algoritmo de clasificación rápida en F#

module QuickSort =

    // Función de clasificación rápida
    let quicksort (lst: int list) =
        // Si la lista está vacía o tiene un solo elemento, se devuelve la lista sin modificar
        if lst.Length <= 1 then
            lst

        else
            // Elegir un elemento pivote (en este caso, el primer elemento de la lista)
            let pivot = lst.[0]

            // Crear una lista de elementos menores que el pivote y una lista de elementos mayores que el pivote
            let menor, mayor = lst.Partition(fun x -> x < pivot)

            // Clasificar recursivamente las listas de elementos menores y mayores que el pivote
            let menoresOrdenados = quicksort menor
            let mayoresOrdenados = quicksort mayor

            // Combinar las listas ordenadas de elementos menores, el pivote y los elementos mayores
            menoresOrdenados + [pivot] + mayoresOrdenados


// Uso del algoritmo de clasificación rápida

let ejemplo = [7; 3; 1; 4; 2; 6; 5]
printfn "Lista sin ordenar: %A" ejemplo

let resultado = QuickSort.quicksort ejemplo
printfn "Lista ordenada: %A" resultado