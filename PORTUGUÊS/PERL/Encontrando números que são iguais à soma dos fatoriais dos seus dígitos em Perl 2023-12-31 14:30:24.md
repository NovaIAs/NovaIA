Claro! Aqui está um exemplo de um código complexo em Perl:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para calcular fatorial de um número
sub fatorial {
    my $numero = shift;
    my $resultado = 1;

    for (my $i = 1; $i <= $numero; $i++) {
        $resultado *= $i;
    }

    return $resultado;
}

# Função para verificar se um número é primo
sub eh_primo {
    my $numero = shift;

    if ($numero <= 1) {
        return 0;
    }

    for (my $i = 2; $i <= sqrt($numero); $i++) {
        if ($numero % $i == 0) {
            return 0;
        }
    }

    return 1;
}

# Função para encontrar os números primos no intervalo [1, n]
sub encontrar_primos {
    my $n = shift;
    my @primos;

    for (my $i = 2; $i <= $n; $i++) {
        if (eh_primo($i)) {
            push @primos, $i;
        }
    }

    return @primos;
}

# Função para calcular a soma dos fatoriais dos dígitos de um número
sub soma_fatoriais_digitos {
    my $numero = shift;
    my @digitos = split //, $numero;
    my $soma = 0;

    foreach my $digito (@digitos) {
        $soma += fatorial($digito);
    }

    return $soma;
}

# Função para verificar se um número é igual à soma dos fatoriais dos seus dígitos
sub eh_igual_soma_fatoriais_digitos {
    my $numero = shift;
    my $soma = soma_fatoriais_digitos($numero);

    return $numero == $soma;
}

# Encontra os números que satisfazem a condição e os imprime
sub encontrar_numeros_soma_fatoriais {
    my $limite = shift;

    for (my $i = 1; $i <= $limite; $i++) {
        if (eh_igual_soma_fatoriais_digitos($i)) {
            print "$i é igual à soma dos fatoriais dos seus dígitos\n";
        }
    }
}

# Execução principal
print "Encontrando números que são iguais à soma dos fatoriais dos seus dígitos:\n";
print "Digite um número limite: ";
my $limite = <STDIN>;
chomp $limite;

encontrar_numeros_soma_fatoriais($limite);
```

Neste código em Perl, temos várias funções que realizam diferentes tarefas. Vamos explicar cada uma delas:

1. `fatorial`: Esta função recebe um número como parâmetro e calcula o seu fatorial.
2. `eh_primo`: Recebe um número como parâmetro e verifica se ele é primo.
3. `encontrar_primos`: Recebe um número como parâmetro e retorna uma lista com todos os números primos até esse número.
4. `soma_fatoriais_digitos`: Recebe um número como parâmetro e calcula a soma dos fatoriais dos seus dígitos.
5. `eh_igual_soma_fatoriais_digitos`: Recebe um número como parâmetro e verifica se ele é igual à soma dos fatoriais dos seus dígitos.
6. `encontrar_numeros_soma_fatoriais`: Recebe um número limite como parâmetro e encontra todos os números que são iguais à soma dos fatoriais dos seus dígitos até esse limite.
7. Por fim, temos o bloco principal que solicita ao usuário digitar um número limite e chama a função `encontrar_numeros_soma_fatoriais` para encontrar e imprimir os números que satisfazem a condição.

Espero que este exemplo atenda às suas expectativas!