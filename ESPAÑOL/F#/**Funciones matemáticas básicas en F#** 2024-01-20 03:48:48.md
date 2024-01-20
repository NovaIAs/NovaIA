```f#

// Definir una función para encontrar el máximo común divisor (MCD) de dos números.
let mcd x y =
    // Si y es 0, entonces el MCD es x.
    if y = 0 then x
    // En caso contrario, el MCD de x e y es el MCD de y y el resto de dividir x entre y.
    else mcd y (x % y)

// Definir una función para encontrar el mínimo común múltiplo (MCM) de dos números.
let mcm x y =
    // El MCM de x e y es x * y / mcd(x, y).
    (x * y) / mcd x y

// Definir una función para encontrar la suma de los primeros n números naturales.
let suma_n_primeros x =
    // La suma de los primeros n números naturales es igual a (n * (n + 1)) / 2.
    ((x * (x + 1)) / 2)

// Definir una función para encontrar el producto de los primeros n números naturales.
let producto_n_primeros x =
    // El producto de los primeros n números naturales es igual a 1 * 2 * 3 * ... * n.
    let rec producto_n_primeros_aux n prod =
        if n = 0 then prod
        else producto_n_primeros_aux (n - 1) (prod * n)
    producto_n_primeros_aux x 1

// Definir una función para encontrar la potencia de un número x elevado a un exponente y.
let potencia_x_y x y =
    // Si y es 0, entonces la potencia de x elevado a y es 1.
    if y = 0 then 1
    // En caso contrario, la potencia de x elevado a y es x * la potencia de x elevado a y-1.
    else x * potencia_x_y x (y - 1)

// Probar las funciones definidas.
let result_mcd = mcd 12 18
let result_mcm = mcm 12 18
let result_suma_n_primeros = suma_n_primeros 5
let result_producto_n_primeros = producto_n_primeros 5
let result_potencia_x_y = potencia_x_y 2 3

// Mostrar los resultados.
printfn "El MCD de 12 y 18 es: %A" result_mcd
printfn "El MCM de 12 y 18 es: %A" result_mcm
printfn "La suma de los primeros 5 números naturales es: %A" result_suma_n_primeros
printfn "El producto de los primeros 5 números naturales es: %A" result_producto_n_primeros
printfn "La potencia de 2 elevado a 3 es: %A" result_potencia_x_y

```
Explicación del código:

* La función `mcd` calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `mcm` calcula el mínimo común múltiplo de dos números utilizando el algoritmo de Euclides.
* La función `suma_n_primeros` calcula la suma de los primeros n números naturales utilizando la fórmula `(n * (n + 1)) / 2`.
* La función `producto_n_primeros` calcula el producto de los primeros n números naturales utilizando la recursividad.
* La función `potencia_x_y` calcula la potencia de un número x elevado a un exponente y utilizando la recursividad.
* La función `printfn` se utiliza para mostrar los resultados en la consola.