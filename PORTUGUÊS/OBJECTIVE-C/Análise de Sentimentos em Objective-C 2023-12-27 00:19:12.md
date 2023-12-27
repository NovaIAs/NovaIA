Claro! Aqui está um código complexo em Objective-C que realiza uma análise de sentimentos em um conjunto de comentários de usuários. Este código utiliza a biblioteca de análise de sentimentos "SentimentAnalysis.h" para calcular a polaridade dos comentários.

```objective-c
#import <Foundation/Foundation.h>
#import "SentimentAnalysis.h"

@interface Comentario : NSObject

@property (nonatomic, strong) NSString *texto;
@property (nonatomic) double polaridade;

@end

@implementation Comentario

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Array de comentários
        NSArray *comentarios = @[@"Adorei o novo produto!",
                                 @"A entrega foi muito rápida.",
                                 @"Não gostei da qualidade do produto.",
                                 @"O atendimento ao cliente foi péssimo.",
                                 @"Estou muito satisfeito com a compra.",
                                 @"Não recomendo esse produto."];
        
        // Array para armazenar os objetos Comentario
        NSMutableArray *arrayComentarios = [[NSMutableArray alloc] init];
        
        // Analisar cada comentário e calcular a polaridade
        for (NSString *comentarioTexto in comentarios) {
            Comentario *comentario = [[Comentario alloc] init];
            comentario.texto = comentarioTexto;
            
            // Calcular a polaridade do comentário
            double polaridade = [SentimentAnalysis calcularPolaridade:comentarioTexto];
            comentario.polaridade = polaridade;
            
            [arrayComentarios addObject:comentario];
        }
        
        // Imprimir os resultados
        for (Comentario *comentario in arrayComentarios) {
            NSLog(@"Comentário: %@", comentario.texto);
            NSLog(@"Polaridade: %f", comentario.polaridade);
            NSLog(@"-----------------------");
        }
    }
    return 0;
}
```

Neste código, começamos importando as bibliotecas necessárias e criando uma classe `Comentario` que possui as propriedades `texto` (que armazena o texto do comentário) e `polaridade` (que armazena a polaridade do comentário).

Em seguida, no método `main`, definimos um array de comentários e um array para armazenar os objetos `Comentario`. Percorremos cada comentário do array utilizando um loop `for` e, para cada comentário, criamos um objeto `Comentario`, definimos seu texto e calculamos a polaridade utilizando a função `calcularPolaridade` da biblioteca de análise de sentimentos.

Por fim, imprimimos os resultados para cada comentário utilizando um loop `for` e a função `NSLog`.

Este código é apenas um exemplo simplificado de como realizar uma análise de sentimentos em Objective-C. A biblioteca `SentimentAnalysis.h` é fictícia e serve apenas para ilustrar a ideia. Em um cenário real, seria necessário utilizar uma biblioteca de análise de sentimentos existente ou implementar a análise de sentimentos manualmente.