```f#
// Crear una función que reciba una lista de números y devuelva el producto de todos ellos.

let productoDeLista (lista: list<int>) =
    if lista.Length = 0 then
        1
    else
        lista.Head * productoDeLista lista.Tail

// Crear una función que reciba una tupla de dos números y devuelva el mayor de ellos.

let mayorDeTupla (tup: tuple<int, int>) =
    if tup.Item1 > tup.Item2 then
        tup.Item1
    else
        tup.Item2

// Crear una función que reciba una matriz de números y devuelva la suma de todos ellos.

let sumaDeMatriz (matriz: array<array<int>>) =
    matriz |> Array.map (fun fila -> fila |> Array.sum) |> Array.sum

// Crear una función que reciba un árbol binario de búsqueda y devuelva una lista con los valores de todos los nodos en orden ascendente.

let recorridoInorden (arbol: tree<int>) =
    match arbol with
    | Leaf -> []
    | Node(izquierda, valor, derecha) -> recorridoInorden izquierda @ [valor] @ recorridoInorden derecha

// Crear una función que reciba una lista de cadenas y devuelva una lista con las cadenas que empiezan por la letra "A".

let cadenasQueEmpiezanPorA (lista: list<string>) =
    lista |> List.filter (fun cadena -> cadena.StartsWith("A"))

```