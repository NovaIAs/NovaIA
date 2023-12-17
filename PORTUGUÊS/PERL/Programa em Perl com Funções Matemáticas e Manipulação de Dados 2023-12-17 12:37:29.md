Claro! Aqui está um código complexo em Perl que é único e explicado em detalhes:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função para calcular o fatorial de um número
sub fatorial {
    my $num = shift;
    my $result = 1;
    for (my $i = 1; $i <= $num; $i++) {
        $result *= $i;
    }
    return $result;
}

# Função para verificar se um número é primo
sub eh_primo {
    my $num = shift;
    if ($num < 2) {
        return 0;
    }
    for (my $i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return 0;
        }
    }
    return 1;
}

# Função para converter um número decimal para binário
sub dec_para_binario {
    my $num = shift;
    my $binario = '';
    while ($num > 0) {
        $binario = ($num % 2) . $binario;
        $num = int($num / 2);
    }
    return $binario;
}

# Função para calcular a média de uma lista de números
sub calcular_media {
    my @numeros = @_;
    my $soma = 0;
    foreach my $num (@numeros) {
        $soma += $num;
    }
    my $media = $soma / scalar(@numeros);
    return $media;
}

# Função para inverter uma string
sub inverter_string {
    my $string = shift;
    my $tamanho = length($string);
    my $invertida = '';
    for (my $i = $tamanho - 1; $i >= 0; $i--) {
        $invertida .= substr($string, $i, 1);
    }
    return $invertida;
}

# Função principal
sub main {
    print "Bem-vindo(a) ao programa em Perl!\n";
    print "Digite um número para calcular seu fatorial: ";
    my $numero = <STDIN>;
    chomp($numero);
    my $fatorial = fatorial($numero);
    print "O fatorial de $numero é $fatorial\n";
    
    print "Digite um número para verificar se é primo: ";
    my $numero2 = <STDIN>;
    chomp($numero2);
    if (eh_primo($numero2)) {
        print "$numero2 é primo\n";
    } else {
        print "$numero2 não é primo\n";
    }
    
    print "Digite um número decimal para converter para binário: ";
    my $numero3 = <STDIN>;
    chomp($numero3);
    my $binario = dec_para_binario($numero3);
    print "O número $numero3 em binário é $binario\n";
    
    print "Digite uma lista de números separados por espaço para calcular a média: ";
    my $entrada = <STDIN>;
    chomp($entrada);
    my @numeros = split(' ', $entrada);
    my $media = calcular_media(@numeros);
    print "A média dos números é $media\n";
    
    print "Digite uma string para inverter: ";
    my $string = <STDIN>;
    chomp($string);
    my $invertida = inverter_string($string);
    print "A string invertida é $invertida\n";
}

main();
```

Explicação do código:

1. Começamos importando os módulos `strict` e `warnings`, que ajudam a identificar possíveis erros e a manter um código mais seguro e legível.

2. Em seguida, definimos uma função chamada `fatorial` que recebe um número como parâmetro e calcula o fatorial desse número utilizando um laço `for` e uma variável `$result` para armazenar o resultado.

3. A próxima função é `eh_primo`, que recebe um número como parâmetro e verifica se ele é primo utilizando um laço `for` e a função `sqrt` para otimizar o processo.

4. A função `dec_para_binario` converte um número decimal para binário utilizando um laço `while` e a função `int` para arredondar o resultado.

5. A função `calcular_media` recebe uma lista de números como parâmetro, calcula a soma desses números utilizando um laço `foreach` e retorna a média.

6. Por fim, a função `inverter_string` recebe uma string como parâmetro, utiliza um laço `for` e a função `substr` para inverter a ordem dos caracteres e retorna a string invertida.

7. A função `main` é a função principal do programa. Ela imprime uma mensagem de boas-vindas e solicita ao usuário que insira um número para calcular seu fatorial, um número para verificar se é primo, um número decimal para converter para binário, uma lista de números separados por espaço para calcular a média e uma string para inverter. As funções definidas anteriormente são chamadas para realizar os cálculos e as respostas são exibidas na tela.

8. Por fim, a função `main` é chamada para iniciar a execução do programa.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, é só pedir.