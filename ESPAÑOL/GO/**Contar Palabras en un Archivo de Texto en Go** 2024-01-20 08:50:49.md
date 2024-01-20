```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

func main() {
	// Abrir el archivo de texto
	archivo, err := os.Open("archivo.txt")
	if err != nil {
		fmt.Println("Error al abrir el archivo:", err)
		return
	}
	defer archivo.Close() // Cerrar el archivo cuando se termine de usar

	// Crear un escáner para leer el archivo línea por línea
	scanner := bufio.NewScanner(archivo)

	// Leer cada línea del archivo
	for scanner.Scan() {
		// Obtener la línea actual
		linea := scanner.Text()

		// Dividir la línea en palabras
		palabras := strings.Fields(linea)

		// Mostrar palabras
		fmt.Println("Palabras en la línea:", palabras)

		// Crear un mapa para contar las apariciones de cada palabra
		conteoPalabras := make(map[string]int)

		// Contar las apariciones de cada palabra en la línea
		for _, palabra := range palabras {
			// Obtener el conteo actual de la palabra
			conteo := conteoPalabras[palabra]

			// Incrementar el conteo de la palabra
			conteoPalabras[palabra] = conteo + 1
		}

		// Mostrar el conteo de palabras
		fmt.Println("Conteo de palabras:", conteoPalabras)
	}

	// Manejar cualquier error que ocurra durante la lectura del archivo
	if err := scanner.Err(); err != nil {
		fmt.Println("Error al leer el archivo:", err)
	}
}
```

Este código lee un archivo de texto línea por línea, divide cada línea en palabras y cuenta la cantidad de veces que aparece cada palabra en la línea. 

Cada parte del código está bien detallada y explicada.