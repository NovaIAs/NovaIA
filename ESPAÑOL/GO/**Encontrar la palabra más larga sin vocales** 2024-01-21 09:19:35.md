**Problema:**
Dada una lista de palabras en español, encontrar la palabra más larga que no contenga ninguna de las vocales AEIOU.

**Solución:**

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Definir la lista de palabras
	palabras := []string{"hola", "mundo", "computadora", "programacion", "algoritmo", "estructura", "datos", "base"}

	// Inicializar la palabra más larga sin vocales
	palabraLargaSinVocales := ""

	// Recorrer la lista de palabras
	for _, palabra := range palabras {
		// Convertir la palabra a minúsculas
		palabra = strings.ToLower(palabra)

		// Verificar si la palabra no contiene ninguna de las vocales
		if !strings.ContainsAny(palabra, "aeiou") {
			// Si la palabra no contiene ninguna vocal
			// Comprobar si es más larga que la palabra más larga sin vocales
			if len(palabra) > len(palabraLargaSinVocales) {
				// Si es más larga, actualizar la palabra más larga sin vocales
				palabraLargaSinVocales = palabra
			}
		}
	}

	// Imprimir la palabra más larga sin vocales
	fmt.Println("La palabra más larga sin vocales es:", palabraLargaSinVocales)
}
```

**Explicación:**

1. **Definir la lista de palabras:**
   Creamos una lista de palabras en español que se utilizará para encontrar la palabra más larga sin vocales.

2. **Inicializar la palabra más larga sin vocales:**
   Inicializamos una variable llamada `palabraLargaSinVocales` con un valor vacío. Esta variable se utilizará para almacenar la palabra más larga sin vocales encontrada hasta ahora.

3. **Recorrer la lista de palabras:**
   Utilizamos un bucle `for` para recorrer la lista de palabras.

4. **Convertir la palabra a minúsculas:**
   Convertimos la palabra actual a minúsculas utilizando la función `strings.ToLower()`. Esto se hace para que la comparación de vocales no sea sensible a las mayúsculas y minúsculas.

5. **Verificar si la palabra no contiene ninguna de las vocales:**
   Utilizamos la función `strings.ContainsAny()` para comprobar si la palabra actual contiene alguna de las vocales "aeiou". Si la palabra no contiene ninguna de las vocales, pasamos al siguiente paso.

6. **Comprobar si la palabra es más larga que la palabra más larga sin vocales:**
   Si la palabra no contiene ninguna vocal, comparamos su longitud con la longitud de la palabra más larga sin vocales actual. Si la palabra es más larga, actualizamos la palabra más larga sin vocales con la palabra actual.

7. **Imprimir la palabra más larga sin vocales:**
   Después de recorrer todas las palabras de la lista, imprimimos la palabra más larga sin vocales utilizando la función `fmt.Println()`.

El código buscará la palabra más larga sin vocales en la lista de palabras definida y la imprimirá.