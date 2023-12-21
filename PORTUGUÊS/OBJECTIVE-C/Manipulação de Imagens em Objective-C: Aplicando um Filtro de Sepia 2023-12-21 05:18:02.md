Claro! Aqui está um exemplo de código complexo em Objective-C que pode ser útil para manipulação de imagens:

```objective-c
#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>

// Definindo uma estrutura para representar uma imagem
typedef struct {
    unsigned char red, green, blue;
} Pixel;

// Função para aplicar um filtro de sepia em uma imagem
void aplicarFiltroSepia(UIImage *imagem) {
    // Obtendo as informações da imagem
    CGImageRef imagemRef = [imagem CGImage];
    NSUInteger largura = CGImageGetWidth(imagemRef);
    NSUInteger altura = CGImageGetHeight(imagemRef);
    NSUInteger bytesPorLinha = largura * 4;
    NSUInteger totalBytes = bytesPorLinha * altura;
    
    // Criando um contexto para manipulação da imagem
    unsigned char *dadosImagem = (unsigned char*)malloc(totalBytes);
    CGContextRef contexto = CGBitmapContextCreate(dadosImagem, largura, altura, 8, bytesPorLinha, CGImageGetColorSpace(imagemRef), kCGImageAlphaPremultipliedLast | kCGBitmapByteOrder32Big);
    CGContextDrawImage(contexto, CGRectMake(0, 0, largura, altura), imagemRef);
    
    // Percorrendo cada pixel da imagem
    for (int i = 0; i < altura; i++) {
        for (int j = 0; j < largura; j++) {
            // Obtendo o pixel atual
            Pixel *pixel = (Pixel*)&dadosImagem[(i * largura + j) * 4];
            
            // Aplicando o filtro de sepia
            unsigned char novoRed = (pixel->red * .393) + (pixel->green * .769) + (pixel->blue * .189);
            unsigned char novoGreen = (pixel->red * .349) + (pixel->green * .686) + (pixel->blue * .168);
            unsigned char novoBlue = (pixel->red * .272) + (pixel->green * .534) + (pixel->blue * .131);
            
            // Atualizando os valores do pixel
            pixel->red = MIN(novoRed, 255);
            pixel->green = MIN(novoGreen, 255);
            pixel->blue = MIN(novoBlue, 255);
        }
    }
    
    // Criando uma nova imagem com os pixels alterados
    CGImageRef novaImagemRef = CGBitmapContextCreateImage(contexto);
    UIImage *novaImagem = [UIImage imageWithCGImage:novaImagemRef];
    
    // Liberando memória
    free(dadosImagem);
    CGContextRelease(contexto);
    CGImageRelease(novaImagemRef);
    
    // Exibindo a nova imagem
    UIImageView *imageView = [[UIImageView alloc] initWithImage:novaImagem];
    [imageView setFrame:CGRectMake(0, 0, largura, altura)];
    [[UIApplication sharedApplication].keyWindow addSubview:imageView];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Carregando a imagem
        UIImage *imagem = [UIImage imageNamed:@"imagem.jpg"];
        
        // Aplicando o filtro de sepia
        aplicarFiltroSepia(imagem);
        
        // Executando o loop de eventos
        [[NSRunLoop currentRunLoop] run];
    }
    return 0;
}
```

Neste exemplo, criamos uma função `aplicarFiltroSepia` que recebe uma imagem como parâmetro e aplica um filtro de sepia nela. Primeiro, obtemos as informações da imagem, criamos um contexto para manipulação da imagem e percorremos cada pixel aplicando o filtro de sepia. Em seguida, criamos uma nova imagem com os pixels alterados e a exibimos em uma imageView na tela. 

Espero que este exemplo tenha sido útil para você! Lembre-se de adaptar o código para suas necessidades específicas.