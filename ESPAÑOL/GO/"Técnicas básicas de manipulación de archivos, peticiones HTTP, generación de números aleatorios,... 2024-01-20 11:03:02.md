```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"math/rand"
	"net/http"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

func main() {
	// Lectura de un archivo
	archivo, err := os.Open("archivo.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer archivo.Close()

	lector := bufio.NewReader(archivo)
	linea, err := lector.ReadString('\n')
	for err == nil {
		fmt.Println(linea)
		linea, err = lector.ReadString('\n')
	}

	// Escritura en un archivo
	archivo, err = os.Create("archivo_nuevo.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer archivo.Close()

	escritor := bufio.NewWriter(archivo)
	escritor.WriteString("Esta es una nueva línea\n")
	escritor.Flush()

	// Peticiones HTTP
	respuesta, err := http.Get("https://www.google.com")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer respuesta.Body.Close()

	cuerpo, err := ioutil.ReadAll(respuesta.Body)
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Println(string(cuerpo))

	// Generación de números aleatorios
	rand.Seed(time.Now().UnixNano())
	numero := rand.Intn(100)
	fmt.Println(numero)

	// Expresiones regulares
	patron := regexp.MustCompile(`\d+`)
	coincidencias := patron.FindStringSubmatch("12345abc67890")
	fmt.Println(coincidencias)

	// Operaciones matemáticas
	fmt.Println(math.Sin(math.Pi / 2))
	fmt.Println(math.Pow(2, 3))

	// Ordenamiento de listas
	lista := []int{1, 5, 3, 7, 2, 4}
	sort.Ints(lista)
	fmt.Println(lista)

	// Conversión de tipos
	numero = 12345
	texto := strconv.Itoa(numero)
	fmt.Println(texto)

	texto = "12345"
	numero, err = strconv.Atoi(texto)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(numero)

	// Cadenas de caracteres
	texto = "1234567890"
	fmt.Println(strings.Contains(texto, "5"))
	fmt.Println(strings.Index(texto, "5"))
	fmt.Println(strings.Split(texto, ""))
}
```