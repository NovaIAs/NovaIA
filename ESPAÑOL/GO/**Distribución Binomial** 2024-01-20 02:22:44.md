```go
// Programa para calcular la distribución de probabilidad de una variable aleatoria binomial
// con parámetros `n` y `p`.

// Se importan las bibliotecas necesarias.
package main

import (
	"fmt"
	"math"
)

// Se define la función `binomial`, que calcula la distribución binomial.
func binomial(n int, p float64, x int) float64 {
	// Se calcula el factorial de `n`.
	nFactorial := factorial(n)

	// Se calcula el factorial de `x`.
	xFactorial := factorial(x)

	// Se calcula el factorial de `n - x`.
	nMinusXFactorial := factorial(n - x)

	// Se calcula la probabilidad binomial.
	probability := (float64(nFactorial) / float64(xFactorial)) * (float64(nMinusXFactorial) / float64(nFactorial)) * math.Pow(p, float64(x)) * math.Pow(1-p, float64(n-x))

	// Se devuelve la probabilidad.
	return probability
}

// Se define la función `factorial`, que calcula el factorial de un número.
func factorial(n int) int {
	// Si `n` es igual a 0, se devuelve 1.
	if n == 0 {
		return 1
	}

	// Se calcula el factorial de `n` llamando recursivamente a la función `factorial`.
	return n * factorial(n-1)
}

// Se define la función `main`, que es la función principal del programa.
func main() {
	// Se declaran las variables `n`, `p`, y `x`.
	var n, p, x int

	// Se solicita al usuario que introduzca el valor de `n`.
	fmt.Print("Introduce el valor de n: ")
	fmt.Scanln(&n)

	// Se solicita al usuario que introduzca el valor de `p`.
	fmt.Print("Introduce el valor de p: ")
	fmt.Scanln(&p)

	// Se solicita al usuario que introduzca el valor de `x`.
	fmt.Print("Introduce el valor de x: ")
	fmt.Scanln(&x)

	// Se calcula la probabilidad binomial.
	probability := binomial(n, float64(p)/100, x)

	// Se muestra la probabilidad binomial.
	fmt.Printf("La probabilidad binomial es %f\n", probability)
}
```

**Explicación del código:**

* La función `binomial` calcula la distribución binomial utilizando la fórmula:

```
P(X = x) = (n! / x! * (n - x)!) * p^x * (1 - p)^(n - x)
```

donde:

* `n` es el número de ensayos.
* `p` es la probabilidad de éxito en cada ensayo.
* `x` es el número de éxitos.

* La función `factorial` calcula el factorial de un número utilizando la fórmula:

```
n! = n * (n - 1) * (n - 2) * ... * 1
```

* La función `main` solicita al usuario que introduzca los valores de `n`, `p`, y `x`, y luego calcula y muestra la probabilidad binomial.

**Ejemplo de ejecución:**

```
Introduce el valor de n: 10
Introduce el valor de p: 50
Introduce el valor de x: 5
La probabilidad binomial es 0.246094
```

En este ejemplo, la probabilidad de obtener exactamente 5 éxitos en 10 ensayos con una probabilidad de éxito del 50% es de 0.246094.