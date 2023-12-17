Claro! Aqui está um exemplo de um código em Java que lida com processamento de imagens, utilizando a biblioteca OpenCV:

```java
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.CvType;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.core.Point;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.RotatedRect;
import org.opencv.core.Rect;
import org.opencv.imgproc.Imgproc;
import org.opencv.imgcodecs.Imgcodecs;

public class ProcessamentoDeImagem {

    public static void main(String[] args) {
        // Carrega a biblioteca OpenCV
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
        
        // Carrega a imagem de entrada
        Mat imagemEntrada = Imgcodecs.imread("imagem.png");
        
        // Converte a imagem para escala de cinza
        Mat imagemCinza = new Mat();
        Imgproc.cvtColor(imagemEntrada, imagemCinza, Imgproc.COLOR_BGR2GRAY);
        
        // Aplica um filtro de suavização para reduzir o ruído
        Mat imagemSuavizada = new Mat();
        Imgproc.GaussianBlur(imagemCinza, imagemSuavizada, new Size(5, 5), 0);
        
        // Detecta as bordas na imagem utilizando o algoritmo de Canny
        Mat bordas = new Mat();
        Imgproc.Canny(imagemSuavizada, bordas, 50, 150);
        
        // Encontra os contornos na imagem
        Mat contornos = new Mat();
        Imgproc.findContours(bordas, contornos, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
        
        // Desenha os contornos na imagem de saída
        Mat imagemSaida = imagemEntrada.clone();
        for (int i = 0; i < contornos.size().height; i++) {
            MatOfPoint contorno = new MatOfPoint(contornos.get(i, 0));
            MatOfPoint2f contorno2f = new MatOfPoint2f(contorno.toArray());
            RotatedRect retanguloRotacionado = Imgproc.minAreaRect(contorno2f);
            Point[] vertices = new Point[4];
            retanguloRotacionado.points(vertices);
            for (int j = 0; j < 4; j++) {
                Imgproc.line(imagemSaida, vertices[j], vertices[(j + 1) % 4], new Scalar(0, 255, 0), 2);
            }
        }
        
        // Salva a imagem de saída
        Imgcodecs.imwrite("imagem_saida.png", imagemSaida);
        
        System.out.println("Processamento de imagem concluído!");
    }
}
```

Neste código, utilizamos a biblioteca OpenCV para realizar o processamento de uma imagem de entrada. Primeiramente, carregamos a imagem utilizando a função `Imgcodecs.imread()` e em seguida a convertemos para escala de cinza utilizando `Imgproc.cvtColor()`. 

Após isso, aplicamos um filtro de suavização com a função `Imgproc.GaussianBlur()` para reduzir o ruído na imagem. Em seguida, utilizamos o algoritmo de detecção de bordas de Canny com a função `Imgproc.Canny()` para encontrar as bordas na imagem.

Em seguida, encontramos os contornos na imagem utilizando a função `Imgproc.findContours()`. Para cada contorno encontrado, utilizamos a função `Imgproc.minAreaRect()` para encontrar um retângulo rotacionado que envolve o contorno. Em seguida, desenhamos os vértices desse retângulo na imagem de saída utilizando a função `Imgproc.line()`.

Por fim, salvamos a imagem de saída utilizando `Imgcodecs.imwrite()` e exibimos uma mensagem informando que o processamento de imagem foi concluído.

Este é apenas um exemplo de código complexo em Java que utiliza a biblioteca OpenCV para processamento de imagens. Existem inúmeras outras possibilidades e funcionalidades que podem ser implementadas utilizando essa biblioteca.