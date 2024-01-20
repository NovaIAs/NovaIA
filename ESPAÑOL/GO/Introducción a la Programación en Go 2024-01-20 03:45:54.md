```go
// Importar las librerías necesarias
import "fmt"
import "math"
import "os"
import "strings"
import "bufio"

// Definir una constante global
const ConstanteGlobal = "Valor Constante"

// Definir una variable global
var VariableGlobal = "Valor Variable"

// Definir un tipo de dato personalizado
type MiTipoPersonalizado struct {
	Nombre string
	Edad int
	Activo bool
}

// Función principal
func main() {
	// Declarar variables locales
	var miVariableLocal1 = "Valor Local 1"
	var miVariableLocal2 int = 2
	var miVariableLocal3 = true

	// Imprimir mensaje en la consola
	fmt.Println("Hola, mundo!")

	// Operaciones matemáticas
	resultadoSuma := miVariableLocal1 + miVariableLocal2
	resultadoResta := miVariableLocal1 - miVariableLocal2
	resultadoMultiplicacion := miVariableLocal1 * miVariableLocal2
	resultadoDivision := miVariableLocal1 / miVariableLocal2
	resultadoModulo := miVariableLocal1 % miVariableLocal2

	// Imprimir resultados de las operaciones matemáticas
	fmt.Println("Suma:", resultadoSuma)
	fmt.Println("Resta:", resultadoResta)
	fmt.Println("Multiplicación:", resultadoMultiplicacion)
	fmt.Println("División:", resultadoDivision)
	fmt.Println("Módulo:", resultadoModulo)

	// Convertir un string a un número
	numeroConvertido, err := strconv.Atoi("123")
	if err != nil {
		fmt.Println("Error al convertir el string a número")
	} else {
		fmt.Println("Número convertido:", numeroConvertido)
	}

	// Convertir un número a un string
	stringConvertido := strconv.Itoa(numeroConvertido)
	fmt.Println("String convertido:", stringConvertido)

	// Operadores lógicos
	resultadoY := miVariableLocal2 > 0 && miVariableLocal3
	resultadoO := miVariableLocal2 < 0 || miVariableLocal3
	resultadoNegacion := !miVariableLocal3

	// Imprimir resultados de los operadores lógicos
	fmt.Println("Resultado Y:", resultadoY)
	fmt.Println("Resultado O:", resultadoO)
	fmt.Println("Resultado Negación:", resultadoNegacion)

	// Sentencia condicional if-else
	if miVariableLocal2 > 0 {
		fmt.Println("La variable miVariableLocal2 es mayor que 0")
	} else {
		fmt.Println("La variable miVariableLocal2 es menor o igual que 0")
	}

	// Sentencia condicional switch-case
	switch miVariableLocal2 {
	case 1:
		fmt.Println("La variable miVariableLocal2 es igual a 1")
	case 2:
		fmt.Println("La variable miVariableLocal2 es igual a 2")
	default:
		fmt.Println("La variable miVariableLocal2 es diferente de 1 y 2")
	}

	// Bucle for
	for i := 0; i < 10; i++ {
		fmt.Println("Valor de i:", i)
	}

	// Bucle while
	i := 0
	while i < 10 {
		fmt.Println("Valor de i:", i)
		i++
	}

	// Bucle do-while
	do {
		fmt.Println("Valor de i:", i)
		i++
	} while i < 10

	// Arrays
	var miArray [5]int = [5]int{1, 2, 3, 4, 5}
	fmt.Println("Elementos del array:", miArray)

	// Slices
	miSlice := miArray[1:4]
	fmt.Println("Elementos del slice:", miSlice)

	// Maps
	miMap := make(map[string]int)
	miMap["Uno"] = 1
	miMap["Dos"] = 2
	miMap["Tres"] = 3
	fmt.Println("Elementos del map:", miMap)

	// Funciones
	resultadoFuncion := miFuncion(miVariableLocal1, miVariableLocal2)
	fmt.Println("Resultado de la función:", resultadoFuncion)

	// Funciones anónimas
	resultadoFuncionAnonima := func(a int, b int) int {
		return a + b
	}(miVariableLocal1, miVariableLocal2)
	fmt.Println("Resultado de la función anónima:", resultadoFuncionAnonima)

	// Funciones recursivas
	resultadoFuncionRecursiva := miFuncionRecursiva(5)
	fmt.Println("Resultado de la función recursiva:", resultadoFuncionRecursiva)

	// Crear un objeto de un tipo de dato personalizado
	miObjeto := MiTipoPersonalizado{
		Nombre: "Juan",
		Edad:   25,
		Activo: true,
	}
	fmt.Println("Objeto personalizado:", miObjeto)

	// Abrir un archivo
	archivo, err := os.Open("archivo.txt")
	if err != nil {
		fmt.Println("Error al abrir el archivo")
	} else {
		// Leer el archivo
		scanner := bufio.NewScanner(archivo)
		for scanner.Scan() {
			fmt.Println("Línea del archivo:", scanner.Text())
		}

		// Cerrar el archivo
		archivo.Close()
	}

	// Tratar errores
	err = miFuncionQuePuedeGenerarErrores()
	if err != nil {
		fmt.Println("Error en la función miFuncionQuePuedeGenerarErrores:", err)
	}

	// Salir del programa
	os.Exit(0)
}

// Función que recibe dos argumentos y devuelve una suma
func miFuncion(a int, b int) int {
	return a + b
}

// Función recursiva que calcula el factorial de un número
func miFuncionRecursiva(n int) int {
	if n == 0 {
		return 1
	} else {
		return n * miFuncionRecursiva(n-1)