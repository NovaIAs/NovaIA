```go
// Programa para calcular el Máximo Común Divisor (MCD) entre dos números enteros.

package main

import "fmt"

// Función para calcular el MCD de dos números enteros.
func mcd(a, b int) int {
    if b == 0 {
        return a
    }
    return mcd(b, a%b)
}

// Función principal.
func main() {
    var a, b int

    // Leer dos números enteros del usuario.
    fmt.Print("Ingrese el primer número: ")
    fmt.Scan(&a)
    fmt.Print("Ingrese el segundo número: ")
    fmt.Scan(&b)

    // Calcular el MCD de los dos números.
    mcd := mcd(a, b)

    // Mostrar el MCD en la consola.
    fmt.Printf("El MCD de %d y %d es %d\n", a, b, mcd)
}
```

Explicación:

* El programa comienza importando el paquete `fmt`, que es necesario para leer y escribir en la consola.
* La función `mcd` calcula el MCD de dos números enteros. La función utiliza el algoritmo de Euclides para calcular el MCD. El algoritmo de Euclides es un algoritmo eficiente para calcular el MCD de dos números.
* La función `main` es la función principal del programa. La función `main` comienza leyendo dos números enteros del usuario. Luego, la función `main` llama a la función `mcd` para calcular el MCD de los dos números. Finalmente, la función `main` muestra el MCD en la consola.