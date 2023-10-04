Claro! Aqui está um código em Perl que implementa uma aplicação de criptografia simétrica utilizando o algoritmo AES (Advanced Encryption Standard):

```perl
use Crypt::Mode::ECB;
use Crypt::Cipher::AES;

sub criptografar {
    my ($chave, $mensagem) = @_;

    # Convertendo a chave para bytes
    my $chave_bytes = pack('H*', $chave);

    # Criando o objeto de cifra AES
    my $cipher = Crypt::Cipher::AES->new($chave_bytes);

    # Convertendo a mensagem para bytes
    my $mensagem_bytes = pack('H*', $mensagem);

    # Criando o objeto do modo de operação ECB
    my $modo = Crypt::Mode::ECB->new('AES');

    # Criptografando a mensagem
    my $mensagem_criptografada = $modo->encrypt($cipher, $mensagem_bytes);

    # Convertendo a mensagem criptografada para hexadecimal
    my $mensagem_hex = unpack('H*', $mensagem_criptografada);

    return $mensagem_hex;
}

sub descriptografar {
    my ($chave, $mensagem_hex) = @_;

    # Convertendo a chave para bytes
    my $chave_bytes = pack('H*', $chave);

    # Criando o objeto de cifra AES
    my $cipher = Crypt::Cipher::AES->new($chave_bytes);

    # Convertendo a mensagem criptografada de hexadecimal para bytes
    my $mensagem_bytes = pack('H*', $mensagem_hex);

    # Criando o objeto do modo de operação ECB
    my $modo = Crypt::Mode::ECB->new('AES');

    # Descriptografando a mensagem
    my $mensagem_descriptografada = $modo->decrypt($cipher, $mensagem_bytes);

    # Convertendo a mensagem descriptografada para hexadecimal
    my $mensagem_descriptografada_hex = unpack('H*', $mensagem_descriptografada);

    return $mensagem_descriptografada_hex;
}

# Exemplo de uso

my $chave = "2b7e151628aed2a6abf7158809cf4f3c";
my $mensagem = "6bc1bee22e409f96e93d7e117393172a";
my $mensagem_criptografada = criptografar($chave, $mensagem);
my $mensagem_descriptografada = descriptografar($chave, $mensagem_criptografada);

print "Mensagem original: $mensagem\n";
print "Mensagem criptografada: $mensagem_criptografada\n";
print "Mensagem descriptografada: $mensagem_descriptografada\n";
```

Nesse código, utilizamos os módulos `Crypt::Mode::ECB` e `Crypt::Cipher::AES` para realizar a criptografia e descriptografia utilizando o algoritmo AES. A função `criptografar` recebe uma chave e uma mensagem em formato hexadecimal e retorna a mensagem criptografada em formato hexadecimal. A função `descriptografar` recebe a mesma chave e a mensagem criptografada (em formato hexadecimal) e retorna a mensagem descriptografada também em formato hexadecimal.

O exemplo de uso demonstra como utilizar as funções, gerando uma mensagem criptografada e posteriormente descriptografando-a novamente. A chave utilizada no exemplo é apenas um exemplo, você pode substituí-la por qualquer chave válida de 128 bits (32 caracteres hexadecimais). A mensagem também é apenas um exemplo, você pode substituí-la por qualquer mensagem que desejar.