Claro! Aqui está um código complexo em Perl, utilizando a linguagem natural em português para explicá-lo:

```perl
use strict;
use warnings;

# Função recursiva para calcular o fatorial de um número
sub fatorial {
    my $numero = shift;
    
    if ($numero <= 1) {
        return 1;
    }
    else {
        return $numero * fatorial($numero - 1);
    }
}

# Função para verificar se um número é primo
sub eh_primo {
    my $numero = shift;
    
    if ($numero < 2) {
        return 0;
    }
    
    for (my $i = 2; $i <= int(sqrt($numero)); $i++) {
        if ($numero % $i == 0) {
            return 0;
        }
    }
    
    return 1;
}

# Função para converter um número decimal para binário
sub decimal_para_binario {
    my $numero = shift;
    
    my $binario = '';
    
    while ($numero > 0) {
        $binario = $numero % 2 . $binario;
        $numero = int($numero / 2);
    }
    
    return $binario;
}

# Função para inverter uma string
sub inverter_string {
    my $string = shift;
    
    my $string_invertida = '';
    
    for (my $i = length($string) - 1; $i >= 0; $i--) {
        $string_invertida .= substr($string, $i, 1);
    }
    
    return $string_invertida;
}

# Função principal
sub main {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp($numero);
    
    print "O fatorial de $numero é " . fatorial($numero) . "\n";
    
    if (eh_primo($numero)) {
        print "$numero é primo\n";
    }
    else {
        print "$numero não é primo\n";
    }
    
    print "O número $numero em binário é " . decimal_para_binario($numero) . "\n";
    
    print "Digite uma string: ";
    my $string = <STDIN>;
    chomp($string);
    
    print "A string invertida é " . inverter_string($string) . "\n";
}

# Executa a função principal
main();
```

Este código em Perl possui algumas funções que realizam diferentes tarefas:

1. A função `fatorial` recebe um número como entrada e calcula o fatorial desse número utilizando recursão.
2. A função `eh_primo` verifica se um número é primo. Ela itera por todos os números menores que a raiz quadrada do número e verifica se algum deles é um divisor. Se encontrar um divisor, o número não é primo.
3. A função `decimal_para_binario` converte um número decimal para binário. Ela utiliza uma abordagem de divisões sucessivas por 2, concatenando os restos em ordem inversa.
4. A função `inverter_string` inverte uma string, iterando por cada caractere da string e concatenando-os em ordem inversa.
5. A função `main` é a função principal do programa. Ela solicita um número ao usuário, calcula o fatorial, verifica se o número é primo e converte-o para binário. Em seguida, solicita uma string e a inverte. Os resultados são exibidos no console.

Espero que este código complexo atenda às suas expectativas!