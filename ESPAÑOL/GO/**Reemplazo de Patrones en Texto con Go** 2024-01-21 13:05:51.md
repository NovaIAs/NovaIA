```go
// Programa que busca un patrón en un texto de prueba y lo reemplaza por otro.
// Uso:
//     $ go run buscar_y_reemplazar.go [patrón] [reemplazo] [ruta_archivo]

package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
)

func main() {
	if len(os.Args) < 4 {
		log.Fatal("Uso: go run buscar_y_reemplazar.go [patrón] [reemplazo] [ruta_archivo]")
	}

	// Obtenemos el patrón y el reemplazo de los argumentos de la línea de comandos.
	patrón := os.Args[1]
	reemplazo := os.Args[2]

	// Obtenemos el contenido del archivo de prueba.
	ruta_archivo := os.Args[3]
	contenidos, err := ioutil.ReadFile(ruta_archivo)
	if err != nil {
		log.Fatal(err)
	}

	// Creamos una expresión regular del patrón.
	expresión_regular := regexp.MustCompile(patrón)

	// Buscamos el patrón en el texto de prueba.
	coincidencias := expresión_regular.FindAllStringIndex(string(contenidos), -1)

	// Reemplazamos el patrón por el reemplazo en el texto de prueba.
	salida := []byte{}
	inicio := 0
	for _, coincidencia := range coincidencias {
		salida = append(salida, contenidos[inicio:coincidencia[0]])
		salida = append(salida, []byte(reemplazo)...)
		inicio = coincidencia[1]
	}
	salida = append(salida, contenidos[inicio:])

	// Guardamos el texto de prueba modificado en un nuevo archivo.
	nueva_ruta_archivo := ruta_archivo + ".modificado"
	err = ioutil.WriteFile(nueva_ruta_archivo, salida, 0644)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("El texto de prueba modificado se guardó en %s.\n", nueva_ruta_archivo)
}
```

Explicación del código:

1. **Lectura de los argumentos de la línea de comandos:**
   - Obtenemos el patrón, el reemplazo y la ruta del archivo de prueba de los argumentos de la línea de comandos.

2. **Lectura del contenido del archivo de prueba:**
   - Leemos el contenido del archivo de prueba en una variable de tipo `[]byte`.

3. **Creación de la expresión regular:**
   - Creamos una expresión regular del patrón utilizando la función `regexp.MustCompile()` para convertir una cadena en una expresión regular.

4. **Búsqueda del patrón en el texto de prueba:**
   - Buscamos el patrón en el texto de prueba utilizando el método `FindAllStringIndex` de la expresión regular. Este método devuelve una lista de índices de las coincidencias del patrón.

5. **Reemplazo del patrón por el reemplazo:**
   - Reemplazamos el patrón por el reemplazo en el texto de prueba utilizando un bucle `for`. Para cada coincidencia del patrón, concatenamos el texto de prueba antes de la coincidencia, el reemplazo y el texto de prueba después de la coincidencia.

6. **Guardado del texto de prueba modificado en un nuevo archivo:**
   - Guardamos el texto de prueba modificado en un nuevo archivo con el nombre `ruta_archivo.modificado`.

7. **Impresión de un mensaje de éxito:**
   - Imprimimos un mensaje de éxito en la consola indicando que el texto de prueba modificado se guardó en el nuevo archivo.