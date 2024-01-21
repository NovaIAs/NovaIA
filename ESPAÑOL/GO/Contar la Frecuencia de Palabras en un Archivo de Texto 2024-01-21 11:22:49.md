```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

func main() {
	// Leer el archivo de texto
	archivo, err := os.Open("archivo.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer archivo.Close()

	// Crear un escáner para leer el archivo línea por línea
	escaner := bufio.NewScanner(archivo)

	// Crear un mapa para almacenar la frecuencia de las palabras
	frecuencia := make(map[string]int)

	// Leer cada línea del archivo y procesarla
	for escaner.Scan() {
		linea := escaner.Text()

		// Eliminar los caracteres de puntuación de la línea
		linea = strings.ReplaceAll(linea, ",", "")
		linea = strings.ReplaceAll(linea, ".", "")
		linea = strings.ReplaceAll(linea, ";", "")
		linea = strings.ReplaceAll(linea, ":", "")
		linea = strings.ReplaceAll(linea, "!", "")
		linea = strings.ReplaceAll(linea, "?", "")

		// Convertir la línea a minúsculas
		linea = strings.ToLower(linea)

		// Dividir la línea en palabras
		palabras := strings.Fields(linea)

		// Recorrer las palabras de la línea
		for _, palabra := range palabras {
			// Eliminar las palabras vacías
			if palabra == "" {
				continue
			}

			// Eliminar las palabras que empiezan por un dígito
			if regexp.MustCompile(`^[0-9]`).MatchString(palabra) {
				continue
			}

			// Incrementar la frecuencia de la palabra
			frecuencia[palabra]++
		}
	}

	// Ordenar las palabras por frecuencia
	type PalabraFrecuencia struct {
		Palabra     string
		Frecuencia int
	}
	var palabrasFrecuencia []PalabraFrecuencia
	for palabra, frecuencia := range frecuencia {
		palabrasFrecuencia = append(palabrasFrecuencia, PalabraFrecuencia{palabra, frecuencia})
	}
	sort.Slice(palabrasFrecuencia, func(i, j int) bool {
		return palabrasFrecuencia[i].Frecuencia > palabrasFrecuencia[j].Frecuencia
	})

	// Mostrar las 10 palabras más frecuentes
	fmt.Println("Las 10 palabras más frecuentes en el archivo son:")
	for i, palabraFrecuencia := range palabrasFrecuencia[:10] {
		fmt.Printf("%d. %s (%d)\n", i+1, palabraFrecuencia.Palabra, palabraFrecuencia.Frecuencia)
	}
}
```

Explicación:

1. La función `main()` es la función principal del programa.

2. La función `os.Open()` se utiliza para abrir el archivo de texto especificado en la ruta "archivo.txt". Si el archivo no existe o no se puede abrir, se produce un error.

3. La función `bufio.NewScanner()` se utiliza para crear un escáner que se utiliza para leer el archivo línea por línea.

4. El mapa `frecuencia` se utiliza para almacenar la frecuencia de cada palabra en el archivo. La clave del mapa es la palabra y el valor es su frecuencia.

5. El bucle `for escaner.Scan()` se utiliza para leer cada línea del archivo y procesarla.

6. La función `strings.ReplaceAll()` se utiliza para eliminar los caracteres de puntuación de la línea.

7. La función `strings.ToLower()` se utiliza para convertir la línea a minúsculas.

8. La función `strings.Fields()` se utiliza para dividir la línea en palabras.

9. El bucle `for _, palabra := range palabras` se utiliza para recorrer las palabras de la línea.

10. La condición `if palabra == ""` se utiliza para eliminar las palabras vacías.

11. La condición `if regexp.MustCompile(`^[0-9]`).MatchString(palabra)` se utiliza para eliminar las palabras que empiezan por un dígito.

12. La función `frecuencia[palabra]++` se utiliza para incrementar la frecuencia de la palabra en el mapa `frecuencia`.

13. La función `sort.Slice()` se utiliza para ordenar las palabras por frecuencia.

14. El bucle `for i, palabraFrecuencia := range palabrasFrecuencia[:10]` se utiliza para mostrar las 10 palabras más frecuentes en el archivo.