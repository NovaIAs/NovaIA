Claro! Aqui está um exemplo de um código complexo em Perl que utiliza uma variedade de recursos e explica cada parte do código:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função para calcular o fatorial de um número
sub fatorial {
    my ($numero) = @_;
    
    # Verifica se o número é válido
    die "Número inválido!" if $numero < 0;
    
    # Caso base: fatorial de 0 é 1
    return 1 if $numero == 0;
    
    # Calcula o fatorial recursivamente
    return $numero * fatorial($numero - 1);
}

# Função para verificar se um número é primo
sub eh_primo {
    my ($numero) = @_;
    
    # Verifica se o número é válido
    die "Número inválido!" if $numero < 2;
    
    # Verifica se o número é divisível por algum número entre 2 e a raiz quadrada do número
    for (my $i = 2; $i <= sqrt($numero); $i++) {
        return 0 if $numero % $i == 0;
    }
    
    return 1;
}

# Função para calcular a sequência de Fibonacci até um determinado limite
sub fibonacci {
    my ($limite) = @_;
    
    # Verifica se o limite é válido
    die "Limite inválido!" if $limite < 1;
    
    my @sequencia = (0, 1);
    
    while ($sequencia[-1] < $limite) {
        push @sequencia, $sequencia[-1] + $sequencia[-2];
    }
    
    return @sequencia;
}

# Função principal
sub main {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;
    
    print "\n";
    print "O fatorial de $numero é: " . fatorial($numero) . "\n";
    
    if (eh_primo($numero)) {
        print "$numero é primo.\n";
    } else {
        print "$numero não é primo.\n";
    }
    
    print "A sequência de Fibonacci até $numero é: " . join(", ", fibonacci($numero)) . "\n";
}

# Chamada da função principal
main();
```

Este código em Perl consiste em um programa que permite ao usuário inserir um número e, em seguida, calcula o fatorial desse número, verifica se ele é primo e mostra a sequência de Fibonacci até esse número. Aqui está uma explicação das principais partes do código:

1. A primeira linha `#!/usr/bin/perl` é conhecida como shebang e indica o caminho para o interpretador Perl.
2. As linhas `use strict` e `use warnings` garantem que o código seja executado de forma estrita e exiba avisos em caso de erros ou variáveis não declaradas.
3. Em seguida, temos três funções: `fatorial`, `eh_primo` e `fibonacci`. Cada uma dessas funções recebe um parâmetro e retorna um valor específico.
4. A função `main` é responsável por solicitar a entrada do usuário, chamar as outras funções e exibir os resultados.
5. A chamada `main()` no final do código inicia a execução do programa.

Espero que este exemplo atenda às suas expectativas!