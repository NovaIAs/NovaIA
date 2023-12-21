Claro! Aqui está um código complexo em F# que lida com processamento de imagens:

```fsharp
open System
open System.Drawing
open System.Drawing.Imaging

type ImagemProcessada = {
    Largura: int
    Altura: int
    Pixels: byte[][]
}

let processarImagem (imagem: Bitmap) =
    let largura = imagem.Width
    let altura = imagem.Height
    let pixels = Array2D.create<byte> altura largura
    
    for x = 0 to largura - 1 do
        for y = 0 to altura - 1 do
            let corPixel = imagem.GetPixel(x, y)
            let tomCinza = (corPixel.R + corPixel.G + corPixel.B) / 3
            pixels.[y, x] <- byte tomCinza
    
    { Largura = largura; Altura = altura; Pixels = pixels }

let salvarImagemProcessada (imagemProcessada: ImagemProcessada) (caminho: string) =
    let imagem = new Bitmap(imagemProcessada.Largura, imagemProcessada.Altura)
    
    for x = 0 to imagemProcessada.Largura - 1 do
        for y = 0 to imagemProcessada.Altura - 1 do
            let tomCinza = int(imagemProcessada.Pixels.[y, x])
            let corPixel = Color.FromArgb(tomCinza, tomCinza, tomCinza)
            imagem.SetPixel(x, y, corPixel)
    
    imagem.Save(caminho, ImageFormat.Png)

[<EntryPoint>]
let main argv =
    let caminhoImagem = "caminho/para/imagem.png"
    let imagemOriginal = new Bitmap(caminhoImagem)
    let imagemProcessada = processarImagem imagemOriginal
    let caminhoImagemProcessada = "caminho/para/imagem_processada.png"
    salvarImagemProcessada imagemProcessada caminhoImagemProcessada
    
    printfn "Imagem processada salva com sucesso em %s" caminhoImagemProcessada
    0
```

Este código em F# processa uma imagem, converte-a para escala de cinza e salva a imagem processada em um novo arquivo. Primeiro, importamos os namespaces necessários para trabalhar com imagens. Em seguida, definimos um tipo de dados `ImagemProcessada` para armazenar informações sobre a imagem processada, incluindo largura, altura e matriz de pixels.

A função `processarImagem` recebe um objeto `Bitmap` representando a imagem original e itera sobre todos os pixels da imagem. Para cada pixel, a função obtém os componentes de cor (R, G, B), calcula a média dos valores e converte para um valor de tom de cinza. A matriz de pixels `pixels` é preenchida com os valores de tom de cinza calculados.

A função `salvarImagemProcessada` recebe um objeto `ImagemProcessada` e um caminho de arquivo, cria um novo objeto `Bitmap` com base nas dimensões da imagem processada e itera sobre todos os pixels da imagem processada. Para cada pixel, a função cria uma cor com o mesmo valor de tom de cinza para os componentes R, G e B e define o pixel correspondente no objeto `Bitmap`. Por fim, a imagem processada é salva no caminho especificado.

No ponto de entrada `main`, um caminho de arquivo para a imagem original é definido. A imagem original é carregada em um objeto `Bitmap`, processada pela função `processarImagem`, e a imagem processada é salva em um novo caminho de arquivo. Uma mensagem é impressa para indicar o sucesso da operação.

Espero que este código atenda às suas expectativas!