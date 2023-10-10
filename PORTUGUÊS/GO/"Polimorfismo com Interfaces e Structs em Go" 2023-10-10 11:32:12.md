Claro! Aqui está um exemplo de código complexo em Go:

```go
package main

import (
	"fmt"
	"math"
)

type Shape interface {
	Area() float64
	Perimeter() float64
}

type Rectangle struct {
	width, height float64
}

func (r Rectangle) Area() float64 {
	return r.width * r.height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.width + r.height)
}

type Circle struct {
	radius float64
}

func (c Circle) Area() float64 {
	return math.Pi * c.radius * c.radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.radius
}

func PrintShapeDetails(s Shape) {
	fmt.Printf("Area: %.2f\n", s.Area())
	fmt.Printf("Perimeter: %.2f\n", s.Perimeter())
}

func main() {
	rect := Rectangle{width: 5, height: 3}
	circle := Circle{radius: 2.5}

	fmt.Println("Rectangle Details:")
	PrintShapeDetails(rect)

	fmt.Println("Circle Details:")
	PrintShapeDetails(circle)
}
```

Este código mostra um exemplo de polimorfismo em Go, utilizando interfaces e structs. 

Começamos definindo a interface `Shape`, que possui dois métodos: `Area()` e `Perimeter()`. Em seguida, criamos duas structs: `Rectangle` e `Circle`, que implementam a interface `Shape`.

Os métodos `Area()` e `Perimeter()` são implementados para cada uma das structs, calculando a área e o perímetro de um retângulo e de um círculo, respectivamente.

A função `PrintShapeDetails()` recebe uma instância de `Shape` como parâmetro e imprime a área e o perímetro da forma.

No `main()`, criamos uma instância de `Rectangle` e de `Circle`, e chamamos a função `PrintShapeDetails()` passando cada uma dessas instâncias como argumento.

Ao executar o código, ele irá imprimir os detalhes do retângulo e do círculo, incluindo a área e o perímetro de cada um.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, fique à vontade para perguntar!