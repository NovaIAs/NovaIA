```
// Crear un programa que reciba un número entero como entrada, luego solicite al usuario que ingrese una cadena de texto, y finalmente, el programa invierta la cadena y la muestre en la consola.

package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    fmt.Println("Ingrese un número entero:")
    reader := bufio.NewReader(os.Stdin)
    input, err := reader.ReadString('\n')
    if err != nil {
        fmt.Println("Error al leer la entrada:", err)
        return
    }

    // Convertir la entrada a un número entero
    number, err := strconv.Atoi(input)
    if err != nil {
        fmt.Println("Error al convertir la entrada a número entero:", err)
        return
    }

    // Solicitar al usuario que ingrese una cadena de texto
    fmt.Println("Ingrese una cadena de texto:")
    input, err = reader.ReadString('\n')
    if err != nil {
        fmt.Println("Error al leer la entrada:", err)
        return
    }

    // Invertir la cadena de texto
    reversedString := reverseString(input)

    // Mostrar la cadena invertida en la consola
    fmt.Println("La cadena invertida es:", reversedString)
}

// Función para invertir una cadena de texto
func reverseString(input string) string {
    // Convertir la cadena en un array de runas
    runes := []rune(input)

    // Iterar sobre el array de runas en orden inverso
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        // Intercambiar las runas en las posiciones i y j
        runes[i], runes[j] = runes[j], runes[i]
    }

    // Convertir el array de runas de nuevo en una cadena
    return string(runes)
}
```

Explicación:

1. El programa solicita al usuario que ingrese un número entero.
2. El programa luego solicita al usuario que ingrese una cadena de texto.
3. El programa invierte la cadena de texto llamando a la función `reverseString`.
4. El programa finalmente muestra la cadena invertida en la consola.

La función `reverseString` recibe una cadena de texto como argumento y devuelve una cadena invertida. La función funciona convirtiendo la cadena en un array de runas, iterando sobre el array en orden inverso e intercambiando las runas en las posiciones i y j. Finalmente, la función convierte el array de runas de nuevo en una cadena y la devuelve.