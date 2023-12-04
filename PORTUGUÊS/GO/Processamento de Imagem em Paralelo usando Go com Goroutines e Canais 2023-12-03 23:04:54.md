Claro! Aqui está um código complexo em Go que utiliza concorrência e canais para processar dados de entrada de forma paralela. Ele é baseado em um exemplo de processamento de imagens, onde cada goroutine recebe uma parte da imagem para processar e, em seguida, os resultados são combinados:

```go
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/jpeg"
	"os"
)

func main() {
	// Carrega a imagem de entrada
	img, err := loadImage("input.jpg")
	if err != nil {
		fmt.Printf("Erro ao carregar a imagem: %v\n", err)
		return
	}

	// Divide a imagem em partes para processar em paralelo
	numParts := 4
	width := img.Bounds().Dx()
	height := img.Bounds().Dy()
	partWidth := width / numParts

	// Cria um canal para receber os resultados parciais
	resultChannel := make(chan *image.RGBA, numParts)

	// Processa cada parte da imagem em goroutines separadas
	for i := 0; i < numParts; i++ {
		startX := i * partWidth
		endX := startX + partWidth

		go processImagePart(img, startX, endX, height, resultChannel)
	}

	// Aguarda o término de todas as goroutines
	for i := 0; i < numParts; i++ {
		result := <-resultChannel

		// Combina os resultados parciais em uma única imagem
		for y := 0; y < height; y++ {
			for x := i * partWidth; x < (i+1)*partWidth; x++ {
				img.Set(x, y, result.At(x, y))
			}
		}
	}

	// Salva a imagem de saída
	err = saveImage(img, "output.jpg")
	if err != nil {
		fmt.Printf("Erro ao salvar a imagem: %v\n", err)
		return
	}

	fmt.Println("Processamento de imagem concluído!")
}

func loadImage(filename string) (image.Image, error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	img, err := jpeg.Decode(file)
	if err != nil {
		return nil, err
	}

	return img, nil
}

func saveImage(img image.Image, filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	err = jpeg.Encode(file, img, nil)
	if err != nil {
		return err
	}

	return nil
}

func processImagePart(img image.Image, startX, endX, height int, resultChannel chan<- *image.RGBA) {
	result := image.NewRGBA(image.Rect(startX, 0, endX, height))

	// Processa a parte da imagem atribuída a esta goroutine
	for y := 0; y < height; y++ {
		for x := startX; x < endX; x++ {
			// Obtém a cor original do pixel
			r, g, b, _ := img.At(x, y).RGBA()

			// Aplica um efeito de negativo invertendo os valores RGB
			negativeR := 255 - uint8(r>>8)
			negativeG := 255 - uint8(g>>8)
			negativeB := 255 - uint8(b>>8)

			// Define a nova cor no resultado
			result.SetRGBA(x-startX, y, color.RGBA{negativeR, negativeG, negativeB, 255})
		}
	}

	resultChannel <- result
}
```

Neste código, primeiro carregamos a imagem de entrada usando a função `loadImage`. Em seguida, dividimos a imagem em partes para processar em paralelo. Cada parte é processada em uma goroutine separada pela função `processImagePart`, que aplica um efeito de negativo na imagem. Os resultados parciais são combinados em uma única imagem na função principal. Por fim, a imagem resultante é salva usando a função `saveImage`.

Espero que esse código complexo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, é só me avisar.