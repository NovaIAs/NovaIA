```f#

// Definición de datos
type List<'T> = | Cons of 'T * List<'T> | Nil

// Funciones auxiliares
let rec length l = match l with
                | Cons (_, xs) -> 1 + length xs
                | Nil -> 0

let rec append l1 l2 = match l1 with
                | Cons (x, xs) -> Cons (x, append xs l2)
                | Nil -> l2

// Funciones principales
let reverse l =
    let rec reverse_aux acc l = match l with
                    | Cons (x, xs) -> reverse_aux (Cons (x, acc)) xs
                    | Nil -> acc
    in reverse_aux Nil l

let concat l =
    let rec concat_aux acc l = match l with
                    | Cons (x, xs) -> concat_aux (append acc x) xs
                    | Nil -> acc
    in concat_aux Nil l

// Ejemplo de uso
let l1 = Cons (1, Cons (2, Cons (3, Nil)))
let l2 = Cons (4, Cons (5, Cons (6, Nil)))
let l3 = reverse l1
let l4 = concat (append l1 l2)

// Impresión de resultados
printfn "Lista original: %A" l1
printfn "Lista invertida: %A" l3
printfn "Lista concatenada: %A" l4
```

Explicación del código:

* La declaración `type List<'T> = | Cons of 'T * List<'T> | Nil` define el tipo de datos `List<'T>`, que representa una lista de elementos de tipo `'T`. Una lista puede ser vacía (`Nil`) o puede contener un elemento (`Cons (x, xs)`) seguido de otra lista (`xs`).
* La función `length l` calcula la longitud de una lista `l`, devolviendo el número de elementos en la lista.
* La función `append l1 l2` concatena dos listas `l1` y `l2`, devolviendo una nueva lista que contiene todos los elementos de `l1` seguidos de todos los elementos de `l2`.
* La función `reverse l` invierte una lista `l`, devolviendo una nueva lista que contiene los elementos de `l` en orden inverso.
* La función `concat l` concatena una lista de listas `l`, devolviendo una nueva lista que contiene todos los elementos de todas las listas en `l`.
* El código de ejemplo al final del script crea tres listas, `l1`, `l2` y `l3`, y las utiliza para demostrar las funciones `reverse`, `concat` y `append`.