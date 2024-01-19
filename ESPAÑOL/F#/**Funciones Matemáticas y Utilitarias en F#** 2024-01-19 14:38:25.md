```f#
// Definición de una función que calcula el factorial de un número
let factorial numero =
    let rec factorialAux acum = function
        | 0 -> acum
        | n -> factorialAux (acum * n) (n-1)
    factorialAux 1 numero

// Definición de una función que calcula el número de Fibonacci de un índice
let fibonacci num =
    match num with
    | 0 | 1 -> num
    | _ -> fibonacci(num-1) + fibonacci(num-2)

// Definición de una función que comprueba si un número es primo
let esprimo num =
    let rec esprimoAux divisor = function
        | 2 -> true
        | n when n mod divisor = 0 -> false
        | n -> esprimoAux (divisor + 1) n
    num > 1 && esprimoAux 2 num

// Definición de una función que imprime un árbol binario
let imprimirArbol arbol =
    let rec imprimirArbolAux nivel arbol =
        match arbol with
        | null -> ()
        | nodo ->
            printn "Nivel %d: %A" nivel nodo
            imprimirArbolAux (nivel + 1) nodo.hijoIzquierdo
            imprimirArbolAux (nivel + 1) nodo.hijoDerecho
    imprimirArbolAux 0 arbol

// Definición de una función que realiza una búsqueda binaria en un arreglo
let buscarBinario arreglo valor =
    let rec buscarBinarioAux inicio fin = function
        | [] -> -1
        | [x] when x = valor -> inicio
        | [_] -> -1
        | x :: xs ->
            let medio = (inicio + fin) / 2
            match x with
            | y when y = valor -> medio
            | y when y < valor -> buscarBinarioAux (medio + 1) fin xs
            | _ -> buscarBinarioAux inicio medio xs
    buscarBinarioAux 0 (Array.length arreglo - 1) arreglo

// Ejemplo de uso del código: calcular el factorial de 5, el número de Fibonacci de 10, comprobar si 17 es primo, imprimir un árbol binario y realizar una búsqueda binaria en un arreglo
printfn "Factorial de 5: %A" (factorial 5)
printfn "Número de Fibonacci de 10: %A" (fibonacci 10)
printfn "¿17 es primo?: %A" (esprimo 17)
let arbol = new NodoBinario(10, new NodoBinario(5, null, null), new NodoBinario(15, null, null))
imprimirArbol arbol
let arreglo = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
printfn "Índice de 13 en el arreglo: %A" (buscarBinario arreglo 13)
```

Explicación:

* La primera función, `factorial`, calcula el factorial de un número utilizando una función recursiva auxiliar.
* La segunda función, `fibonacci`, calcula el número de Fibonacci de un índice utilizando recursión.
* La tercera función, `esprimo`, comprueba si un número es primo utilizando una función recursiva auxiliar.
* La cuarta función, `imprimirArbol`, imprime un árbol binario en forma de árbol invertido, con el nivel de cada nodo indicado.
* La quinta función, `buscarBinario`, realiza una búsqueda binaria en un arreglo para encontrar el índice de un valor dado.
* La última parte del código es un ejemplo de uso de las funciones anteriores. Calcula el factorial de 5, el número de Fibonacci de 10, comprueba si 17 es primo, imprime un árbol binario y realiza una búsqueda binaria en un arreglo.