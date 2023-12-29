Claro! Aqui está um exemplo de um código complexo em Python que envolve o processamento de imagens e a aplicação de filtros utilizando a biblioteca OpenCV:

```python
import cv2

def aplicar_filtros(imagem):
    # Converter a imagem para escala de cinza
    imagem_cinza = cv2.cvtColor(imagem, cv2.COLOR_BGR2GRAY)
    
    # Aplicar filtro de suavização Gaussiana
    imagem_suavizada = cv2.GaussianBlur(imagem_cinza, (5, 5), 0)
    
    # Detectar bordas na imagem
    imagem_bordas = cv2.Canny(imagem_suavizada, 50, 150)
    
    # Aplicar filtro de dilatação nas bordas
    elemento_estruturante = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    imagem_dilatada = cv2.dilate(imagem_bordas, elemento_estruturante, iterations=2)
    
    # Encontrar contornos na imagem
    contornos, _ = cv2.findContours(imagem_dilatada.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    
    # Desenhar contornos na imagem original
    cv2.drawContours(imagem, contornos, -1, (0, 255, 0), 2)
    
    # Exibir a imagem com os contornos
    cv2.imshow("Imagem com contornos", imagem)
    cv2.waitKey(0)
    cv2.destroyAllWindows()

def main():
    # Carregar a imagem
    imagem = cv2.imread("imagem.jpg")
    
    # Verificar se a imagem foi carregada corretamente
    if imagem is None:
        print("Erro ao carregar a imagem.")
        return
    
    # Aplicar filtros na imagem
    aplicar_filtros(imagem)

if __name__ == "__main__":
    main()
```

Neste código, utilizamos a biblioteca OpenCV para realizar o processamento de imagens. Primeiro, convertemos a imagem colorida para escala de cinza. Em seguida, aplicamos um filtro de suavização Gaussiana para reduzir o ruído na imagem. Depois, utilizamos o algoritmo de detecção de bordas de Canny para identificar as bordas na imagem.

Para realçar as bordas, aplicamos um filtro de dilatação. Em seguida, utilizamos a função `cv2.findContours()` para encontrar os contornos na imagem dilatada. Por fim, desenhamos os contornos encontrados na imagem original e exibimos a imagem resultante em uma janela.

Para executar o código, é necessário ter a biblioteca OpenCV instalada e uma imagem chamada "imagem.jpg" no mesmo diretório do arquivo Python.