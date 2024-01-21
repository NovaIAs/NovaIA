```f#
// Definición de la función factorial.
let factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

// Definición de la función fibonnacci.
let fib n =
    if n < 2 then n
    else fib (n - 1) + fib (n - 2)

// Definición de la función potencia.
let potencia base exponente =
    if exponente = 0 then 1
    else base * potencia base (exponente - 1)

// Definición de la función mayor divisor común.
let mcd a b =
    if b = 0 then a
    else mcd b (a % b)

// Definición de la función menor múltiplo común.
let mmc a b =
    a * b / mcd a b

// Definición de la función de ordenamiento por burbuja.
let ordenamiento_por_burbuja lista =
    let rec ordenar lista =
        let cambiada = false
        let nueva = List.map (fun e -> e) lista
        for i = 0 to lista.Length - 2 do
            if lista.[i] > lista.[i + 1] then
                let tmp = nueva.[i]
                nueva.[i] <- nueva.[i + 1]
                nueva.[i + 1] <- tmp
                cambiada <- true
        if cambiada then ordenar nueva
        else lista
    ordenar lista

// Definición de la función de búsqueda binaria.
let busqueda_binaria lista elemento =
    let rec buscar lista elemento inicio fin =
        if inicio > fin then -1
        else
            let medio = (inicio + fin) / 2
            if lista.[medio] = elemento then medio
            else if lista.[medio] < elemento then buscar lista elemento (medio + 1) fin
            else buscar lista elemento inicio (medio - 1)
    buscar lista elemento 0 (lista.Length - 1)

// Definición de la función de ordenamiento por selección.
let ordenamiento_por_seleccion lista =
    let rec ordenar lista =
        let minimo = List.min lista
        let nueva = List.filter (fun e -> e <> minimo) lista
        minimo :: ordenar nueva
    ordenar lista

// Definición de la función de ordenamiento por inserción.
let ordenamiento_por_insercion lista =
    let rec ordenar lista =
        match lista with
        | [] -> []
        | cabeza :: cola -> insertar cabeza (ordenar cola)
    and insertar elemento lista =
        let rec insertar_elemento elemento lista =
            match lista with
            | [] -> [elemento]
            | cabeza :: cola ->
                if cabeza > elemento then elemento :: cabeza :: cola
                else cabeza :: insertar_elemento elemento cola
        insertar_elemento elemento lista
    ordenar lista

// Definición de la función de ordenamiento por mezcla.
let ordenamiento_por_mezcla lista =
    let rec mezclar lista1 lista2 =
        match lista1, lista2 with
        | [], _ -> lista2
        | _, [] -> lista1
        | cabeza1 :: cola1, cabeza2 :: cola2 ->
            if cabeza1 < cabeza2 then cabeza1 :: mezclar cola1 lista2
            else cabeza2 :: mezclar lista1 cola2
    let rec ordenar lista =
        match lista with
        | [] -> []
        | [elemento] -> [elemento]
        | _ ->
            let mitad = lista.Length / 2
            let (lista1, lista2) = List.splitAt mitad lista
            mezclar (ordenar lista1) (ordenar lista2)
    ordenar lista

// Definición de la función de ordenamiento rápido.
let ordenamiento_rapido lista =
    let rec ordenar lista =
        match lista with
        | [] -> []
        | [elemento] -> [elemento]
        | _ ->
            let pivote = lista.[lista.Length / 2]
            let menores = List.filter (fun e -> e < pivote) lista
            let iguales = List.filter (fun e -> e = pivote) lista
            let mayores = List.filter (fun e -> e > pivote) lista
            menores @ iguales @ mayores
    ordenar lista
```

Este código implementa varias funciones matemáticas y algoritmos de ordenamiento.

- La función factorial calcula el factorial de un número.
- La función fibonnacci calcula el n-ésimo número de Fibonacci.
- La función potencia calcula la potencia de un número.
- La función mcd calcula el máximo divisor común de dos números.
- La función mmc calcula el mínimo múltiplo común de dos números.
- La función ordenamiento_por_burbuja implementa el ordenamiento por burbuja de una lista.
- La función busqueda_binaria implementa la búsqueda binaria en una lista ordenada.
- La función ordenamiento_por_seleccion implementa el ordenamiento por selección de una lista.
- La función ordenamiento_por_insercion implementa el ordenamiento por inserción de una lista.
- La función ordenamiento_por_mezcla implementa el ordenamiento por mezcla de una lista.
- La función ordenamiento_rapido implementa el ordenamiento rápido de una lista.