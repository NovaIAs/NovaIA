```swift
// Definición de Variables
var n1: Int = 0
var n2: Int = 0
var operacion: String = ""
var resultado: Int = 0

// Función para obtener los números del usuario
func obtenerNumeros() {
    print("Ingrese el primer número: ")
    n1 = Int(readLine()!)!
    print("Ingrese el segundo número: ")
    n2 = Int(readLine()!)!
}

// Función para elegir la operación a realizar
func elegirOperacion() {
    print("Elija la operación a realizar:")
    print("1. Suma")
    print("2. Resta")
    print("3. Multiplicación")
    print("4. División")
    operacion = readLine()!
}

// Función para realizar la operación especificada
func realizarOperacion() {
    switch operacion {
    case "1":
        resultado = n1 + n2
        break
    case "2":
        resultado = n1 - n2
        break
    case "3":
        resultado = n1 * n2
        break
    case "4":
        resultado = n1 / n2
        break
    default:
        print("Operación no válida")
    }
}

// Función para mostrar el resultado
func mostrarResultado() {
    print("El resultado es: \(resultado)")
}

// Función principal
func main() {
    obtenerNumeros()
    elegirOperacion()
    realizarOperacion()
    mostrarResultado()
}

// Invocación de la función principal
main()
```
Explicación del código:

1. Definición de Variables:
   - `n1` y `n2`: Variables para almacenar los dos números ingresados por el usuario.
   - `operacion`: Variable para almacenar la operación a realizar (+, -, *, /).
   - `resultado`: Variable para almacenar el resultado de la operación.

2. Función `obtenerNumeros()`:
   - Obtiene los dos números del usuario a través de la entrada estándar.

3. Función `elegirOperacion()`:
   - Muestra un menú de opciones y permite al usuario elegir la operación a realizar.

4. Función `realizarOperacion()`:
   - Realiza la operación especificada utilizando la variable `operacion` y los valores de `n1` y `n2`.

5. Función `mostrarResultado()`:
   - Muestra el resultado de la operación en la salida estándar.

6. Función `main()`:
   - Invoca las funciones `obtenerNumeros()`, `elegirOperacion()`, `realizarOperacion()` y `mostrarResultado()` para realizar el cálculo.