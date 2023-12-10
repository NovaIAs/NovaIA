Claro! Aqui está um código em Objective-C que implementa um algoritmo avançado de criptografia RSA:

```objective-c
#import <Foundation/Foundation.h>

@interface RSAEncryption : NSObject

@property (nonatomic, strong) NSNumber *p;
@property (nonatomic, strong) NSNumber *q;
@property (nonatomic, strong) NSNumber *n;
@property (nonatomic, strong) NSNumber *e;
@property (nonatomic, strong) NSNumber *d;

- (void)generateKeyPairWithPrimeNumbersP:(NSNumber *)p q:(NSNumber *)q;
- (NSNumber *)encryptMessage:(NSNumber *)message;
- (NSNumber *)decryptMessage:(NSNumber *)encryptedMessage;

@end

@implementation RSAEncryption

- (void)generateKeyPairWithPrimeNumbersP:(NSNumber *)p q:(NSNumber *)q {
    self.p = p;
    self.q = q;
    
    // Calculating n
    self.n = @([self.p intValue] * [self.q intValue]);
    
    // Calculating phi(n)
    NSNumber *phi = @(([self.p intValue] - 1) * ([self.q intValue] - 1));
    
    // Finding e such that 1 < e < phi(n) and gcd(e, phi(n)) = 1
    for (int i = 2; i < [phi intValue]; i++) {
        if ([self gcd:i number2:[phi intValue]] == 1) {
            self.e = @(i);
            break;
        }
    }
    
    // Finding d such that (d * e) % phi(n) = 1
    self.d = [self modInverse:self.e modulus:phi];
}

- (NSNumber *)encryptMessage:(NSNumber *)message {
    // Encrypting the message using the public key (e, n)
    return @([message intValue] ^ [self.e intValue] % [self.n intValue]);
}

- (NSNumber *)decryptMessage:(NSNumber *)encryptedMessage {
    // Decrypting the encrypted message using the private key (d, n)
    return @([encryptedMessage intValue] ^ [self.d intValue] % [self.n intValue]);
}

#pragma mark - Helper Methods

- (NSNumber *)gcd:(int)number1 number2:(int)number2 {
    if (number2 == 0) {
        return @(number1);
    }
    return [self gcd:number2 number2:(number1 % number2)];
}

- (NSNumber *)modInverse:(NSNumber *)a modulus:(NSNumber *)mod {
    int m0 = [mod intValue];
    int y = 0, x = 1;
    
    if ([mod intValue] == 1) {
        return @0;
    }
    
    while ([a intValue] > 1) {
        int q = [a intValue] / [mod intValue];
        int t = [mod intValue];
        
        [mod setIntValue:[a intValue] % [mod intValue]];
        [a setIntValue:t];
        t = y;
        
        y = x - q * y;
        x = t;
    }
    
    if (x < 0) {
        x += m0;
    }
    
    return @(x);
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        RSAEncryption *rsa = [[RSAEncryption alloc] init];
        
        [rsa generateKeyPairWithPrimeNumbersP:@17 q:@23];
        
        NSNumber *message = @42;
        NSNumber *encryptedMessage = [rsa encryptMessage:message];
        NSNumber *decryptedMessage = [rsa decryptMessage:encryptedMessage];
        
        NSLog(@"Original Message: %@", message);
        NSLog(@"Encrypted Message: %@", encryptedMessage);
        NSLog(@"Decrypted Message: %@", decryptedMessage);
    }
    return 0;
}
```

Neste código, implementamos a classe `RSAEncryption` que é responsável por gerar um par de chaves RSA, criptografar uma mensagem utilizando a chave pública e descriptografar a mensagem utilizando a chave privada.

A função `generateKeyPairWithPrimeNumbersP:q:` recebe dois números primos `p` e `q`, calcula `n` (produto dos primos `p` e `q`), calcula `phi(n)` (função totiente de Euler), encontra um número `e` que seja coprimo com `phi(n)` e encontra um número `d` tal que `(d * e) % phi(n) = 1`.

A função `encryptMessage:` recebe uma mensagem como parâmetro e criptografa a mensagem utilizando a chave pública (expoente `e` e módulo `n`) através da operação de exponenciação modular.

A função `decryptMessage:` recebe uma mensagem criptografada como parâmetro e descriptografa a mensagem utilizando a chave privada (expoente `d` e módulo `n`) também através da operação de exponenciação modular.

No `main` do programa, criamos uma instância de `RSAEncryption` e geramos um par de chaves utilizando os números primos `17` e `23`. Em seguida, criptografamos uma mensagem com o valor `42` e descriptografamos a mensagem criptografada. Por fim, exibimos a mensagem original, a mensagem criptografada e a mensagem descriptografada no console.

Este código é um exemplo simples de implementação do algoritmo de criptografia RSA em Objective-C. Vale ressaltar que a implementação real do algoritmo RSA é muito mais complexa e envolve considerações adicionais de segurança, como o uso de números primos grandes e seguros, entre outras medidas.