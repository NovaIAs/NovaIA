Claro! Aqui está um exemplo de um código complexo em Perl:

```perl
#!/usr/bin/perl

# Função para calcular o fatorial de um número
sub fatorial {
    my $num = shift;
    if ($num < 0) {
        return -1; # Retorna -1 se o número for negativo
    }
    elsif ($num == 0) {
        return 1; # Retorna 1 se o número for zero
    }
    else {
        return $num * fatorial($num - 1); # Chamada recursiva para calcular o fatorial
    }
}

# Função para verificar se um número é primo
sub primo {
    my $num = shift;
    if ($num <= 1) {
        return 0; # Retorna falso se o número for menor ou igual a 1
    }
    for (my $i = 2; $i <= sqrt($num); $i++) {
        if ($num % $i == 0) {
            return 0; # Retorna falso se o número for divisível por algum número entre 2 e sua raiz quadrada
        }
    }
    return 1; # Retorna verdadeiro se o número for primo
}

# Função para somar os dígitos de um número
sub soma_digitos {
    my $num = shift;
    my $soma = 0;
    while ($num > 0) {
        $soma += $num % 10; # Adiciona o último dígito ao total
        $num /= 10; # Remove o último dígito
        $num = int($num); # Converte para inteiro
    }
    return $soma;
}

# Função principal
sub main {
    print "Digite um número inteiro positivo: ";
    my $numero = <STDIN>; # Lê o número fornecido pelo usuário
    chomp $numero; # Remove o caractere de nova linha

    # Verifica se o número é positivo
    if ($numero =~ /^\d+$/) {
        # Calcula o fatorial
        my $fatorial = fatorial($numero);
        print "O fatorial de $numero é $fatorial\n";

        # Verifica se o número é primo
        if (primo($numero)) {
            print "$numero é um número primo\n";
        }
        else {
            print "$numero não é um número primo\n";
        }

        # Calcula a soma dos dígitos
        my $soma = soma_digitos($numero);
        print "A soma dos dígitos de $numero é $soma\n";
    }
    else {
        print "Erro: o valor fornecido não é um número inteiro positivo\n";
    }
}

# Chama a função principal
main();
```

Neste código em Perl, temos três funções principais: `fatorial`, `primo` e `soma_digitos`. A função `fatorial` calcula o fatorial de um número, a função `primo` verifica se um número é primo e a função `soma_digitos` soma os dígitos de um número.

No corpo da função principal `main`, o usuário é solicitado a digitar um número inteiro positivo. Em seguida, o código verifica se o número é positivo e chama as três funções para calcular o fatorial, verificar se o número é primo e calcular a soma dos dígitos. Os resultados são exibidos na saída padrão.

Espero que esse código complexo em Perl atenda às suas expectativas!