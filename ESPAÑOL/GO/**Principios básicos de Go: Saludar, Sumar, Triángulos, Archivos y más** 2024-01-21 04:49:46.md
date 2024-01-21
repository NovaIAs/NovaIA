```go
package principal

import (
  "fmt"
  "strconv"
  "strings"
  "math"
  "time"
  "sort"
  "os"
  "bufio"
)

// Definición de tipos
type Persona struct {
  nombre string
  edad int
  sexo string
}

type Punto struct {
  x float64
  y float64
  z float64
}

type Triángulo struct {
  v1 Punto
  v2 Punto
  v3 Punto
}

// Funciones
func saludar(nombre string) {
  fmt.Printf("Hola, %s!\n", nombre)
}

func suma(a, b int) int {
  return a + b
}

func areaTriángulo(t Triángulo) float64 {
  var lado1, lado2, lado3 float64
  lado1 = math.Sqrt(math.Pow(t.v2.x-t.v1.x, 2) + math.Pow(t.v2.y-t.v1.y, 2) + math.Pow(t.v2.z-t.v1.z, 2))
  lado2 = math.Sqrt(math.Pow(t.v3.x-t.v2.x, 2) + math.Pow(t.v3.y-t.v2.y, 2) + math.Pow(t.v3.z-t.v2.z, 2))
  lado3 = math.Sqrt(math.Pow(t.v1.x-t.v3.x, 2) + math.Pow(t.v1.y-t.v3.y, 2) + math.Pow(t.v1.z-t.v3.z, 2))
  semiperímetro := (lado1 + lado2 + lado3) / 2
  return math.Sqrt(semiperímetro * (semiperímetro - lado1) * (semiperímetro - lado2) * (semiperímetro - lado3))
}

func leerArchivo(nombreArchivo string) ([]string, error) {
  archivo, err := os.Open(nombreArchivo)
  if err != nil {
    return nil, err
  }
  defer archivo.Close()

  var líneas []string

  escáner := bufio.NewScanner(archivo)
  for escáner.Scan() {
    líneas = append(líneas, escáner.Text())
  }

  return líneas, nil
}

// Main
func main() {
  // Crear una persona
  persona1 := Persona{"Juan", 25, "Masculino"}

  // Saludar a la persona
  saludar(persona1.nombre)

  // Sumar dos números
  resultado := suma(5, 10)
  fmt.Printf("La suma de 5 y 10 es %d\n", resultado)

  // Calcular el área de un triángulo
  triángulo1 := Triángulo{Punto{0, 0, 0}, Punto{4, 0, 0}, Punto{0, 3, 0}}
  fmt.Printf("El área del triángulo es %.2f\n", areaTriángulo(triángulo1))

  // Leer un archivo de texto
  líneas, err := leerArchivo("archivo.txt")
  if err != nil {
    fmt.Println(err)
    return
  }

  for _, línea := range líneas {
    fmt.Println(línea)
  }

  // Ordenar una lista de números
  lista := []int{5, 3, 1, 2, 4}
  sort.Ints(lista)
  fmt.Println(lista)

  // Obtener la fecha actual
  fechaActual := time.Now()
  fmt.Println(fechaActual.Format("02/01/2006"))

  // Crear un canal
  canal := make(chan int)

  // Goroutine
  go func() {
    for i := 0; i < 10; i++ {
      canal <- i
    }
    close(canal)
  }()

  // Recibir valores del canal
  for valor := range canal {
    fmt.Println(valor)
  }
}
```

Explicación:

* El código define un tipo de datos personalizado llamado `Persona` con los campos `nombre`, `edad` y `sexo`.
* También define un tipo de datos personalizado llamado `Punto` con los campos `x`, `y` y `z`.
* El código define un tipo de datos personalizado llamado `Triángulo` con los campos `v1`, `v2` y `v3`, que son puntos.
* El código define varias funciones, incluyendo `saludar`, `suma`, `areaTriángulo`, `leerArchivo`, `ordenarLista` y `obtenerFechaActual`.
* La función `saludar` toma un nombre como argumento y saluda a la persona con ese nombre.
* La función `suma` toma dos números como argumentos y devuelve la suma de los dos números.
* La función `areaTriángulo` toma un triángulo como argumento y devuelve el área del triángulo.
* La función `leerArchivo` toma el nombre de un archivo como argumento y devuelve un array de cadenas con las líneas del archivo.
* La función `ordenarLista` toma una lista de números como argumento y ordena la lista en orden ascendente.
* La función `obtenerFechaActual` devuelve la fecha y hora actuales en formato "02/01/2006".
* El código crea una variable llamada `persona1` de tipo `Persona` y la inicializa con los valores "Juan", 25 y "Masculino".
* El código llama a la función `saludar` con el nombre de la persona1 como argumento para saludar a la persona.
* El código llama a la función `suma` con los números 5 y 10 como argumentos y guarda el resultado en la variable `resultado`.
* El código imprime el valor de la variable `resultado` en la consola.
* El código crea una variable llamada `triángulo1` de tipo `Triángulo` y la inicializa con los puntos (0, 0, 0), (4, 0, 0) y (0, 3, 0).
* El código llama a la función `areaTriángulo` con el triángulo1 como argumento y guarda el resultado en la variable `área`.
* El código imprime el valor de la variable `área` en la consola.
* El código llama a la función `leerArchivo` con el nombre del archivo "archivo.txt" como argumento y guarda el resultado en la variable `líneas`.
* El código imprime cada una de las líneas del archivo en la consola.
* El código crea una variable llamada `lista` de tipo lista de enteros y la inicializa con los valores 5, 3, 1, 2 y 4.
* El código llama a la función `ordenarLista` con la lista como argumento, lo que ordena la lista en orden ascendente.
* El código imprime la lista ordenada en la consola.
* El código llama a la función `obtenerFechaActual` y guarda el resultado en la variable `fechaActual`.
* El código imprime el valor de la variable `fechaActual` en la consola.
* El código crea un canal de enteros llamado `canal`.
* El código crea una goroutine que envía los números del 0 al 9 al canal.
* El código recibe los valores del canal y los imprime en la consola.