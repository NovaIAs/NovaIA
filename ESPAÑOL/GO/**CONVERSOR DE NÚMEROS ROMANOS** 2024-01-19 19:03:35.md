```go
// Programa para convertir números arábigos en números romanos

// Conversión de números arábigos a romanos
func arabicToRoman(number int) string {
	// Tabla de números romanos y sus valores árabes
	romanNumerals := []struct {
		Arabic int
		Roman  string
	}{
		{1000, "M"},
		{900, "CM"},
		{500, "D"},
		{400, "CD"},
		{100, "C"},
		{90, "XC"},
		{50, "L"},
		{40, "XL"},
		{10, "X"},
		{9, "IX"},
		{5, "V"},
		{4, "IV"},
		{1, "I"},
	}

	// Inicializar el resultado
	var result string

	// Recorrer la tabla de números romanos
	for _, numeral := range romanNumerals {
		// Mientras el número sea mayor o igual que el valor árabe del número romano actual
		for number >= numeral.Arabic {
			// Añadir el número romano actual al resultado
			result += numeral.Roman

			// Restar el valor árabe del número romano actual del número
			number -= numeral.Arabic
		}
	}

	// Devolver el resultado
	return result
}

// Función principal
func main() {
	// Crear un mapa para almacenar los números arábigos y sus representaciones romanas
	numbers := map[int]string{
		1:    "I",
		4:    "IV",
		5:    "V",
		9:    "IX",
		10:   "X",
		40:   "XL",
		50:   "L",
		90:   "XC",
		100:  "C",
		400:  "CD",
		500:  "D",
		900:  "CM",
		1000: "M",
	}

	// Crear un mapa para almacenar los números romanos y sus valores arábigos
	romanNumerals := make(map[string]int)
	for number, roman := range numbers {
		romanNumerals[roman] = number
	}

	// Solicitar al usuario que ingrese un número romano
	fmt.Printf("Ingrese un número romano: ")
	var romanNumber string
	fmt.Scanln(&romanNumber)

	// Convertir el número romano a un número arábigo
	if number, ok := romanNumerals[romanNumber]; ok {
		fmt.Printf("El número arábigo equivalente es: %d\n", number)
	} else {
		fmt.Printf("El número romano ingresado no es válido.\n")
	}

	// Convertir un número arábigo a un número romano
	fmt.Printf("Ingrese un número arábigo: ")
	var arabicNumber int
	fmt.Scanln(&arabicNumber)

	if arabicNumber > 0 && arabicNumber <= 3999 {
		romanNumber := arabicToRoman(arabicNumber)
		fmt.Printf("El número romano equivalente es: %s\n", romanNumber)
	} else {
		fmt.Printf("El número arábigo ingresado debe estar entre 1 y 3999.\n")
	}
}
```

**Explicación:**

* El programa comienza definiendo una tabla de números romanos y sus valores árabes.
* Luego, define la función `arabicToRoman` que convierte un número arábigo en un número romano.
* La función `arabicToRoman` recorre la tabla de números romanos y añade el número romano actual al resultado mientras el número sea mayor o igual que el valor árabe del número romano actual.
* A continuación, el programa define la función `main` que solicita al usuario que ingrese un número romano y lo convierte a un número arábigo.
* Si el número romano ingresado es válido, el programa imprime el número arábigo equivalente.
* Luego, el programa solicita al usuario que ingrese un número arábigo y lo convierte a un número romano.
* Si el número arábigo ingresado está entre 1 y 3999, el programa imprime el número romano correspondiente.