Claro! Aqui está um exemplo de um código complexo em Objective-C que lida com manipulação de strings e arrays:

```objective-c
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        
        // Criar um array de strings
        NSArray *nomes = @[@"João", @"Maria", @"Pedro", @"Ana", @"Lucas"];
        
        // Inicializar um dicionário vazio
        NSMutableDictionary *dicionario = [[NSMutableDictionary alloc] init];
        
        // Percorrer o array de nomes
        for (NSString *nome in nomes) {
            
            // Criar uma chave única baseada no nome
            NSString *chave = [[[nome stringByReplacingOccurrencesOfString:@" " withString:@""] lowercaseString] stringByAppendingString:@"_id"];
            
            // Gerar um ID aleatório de 6 dígitos
            NSString *idAleatorio = [NSString stringWithFormat:@"%06d", arc4random_uniform(1000000)];
            
            // Adicionar o nome e o ID ao dicionário
            [dicionario setObject:idAleatorio forKey:chave];
        }
        
        // Imprimir o dicionário
        NSLog(@"Dicionário: %@", dicionario);
    }
    return 0;
}
```

Neste código, primeiro importamos a biblioteca Foundation, que é necessária para usar os recursos do Objective-C. Em seguida, na função `main`, criamos um array chamado `nomes` com alguns nomes.

Então, inicializamos um dicionário vazio chamado `dicionario`. Em seguida, percorremos o array `nomes` usando um loop `for-in`. Para cada nome, criamos uma chave única removendo espaços em branco e convertendo para letras minúsculas. Em seguida, concatenamos "_id" para tornar a chave ainda mais exclusiva.

Depois, geramos um ID aleatório de 6 dígitos usando a função `arc4random_uniform`. Em seguida, adicionamos o nome e o ID ao dicionário usando o método `setObject:forKey:`.

Por fim, imprimimos o dicionário usando o comando `NSLog`.