Claro! Aqui está um código complexo em PERL, escrito em português, que envolve a criação de uma calculadora interativa com várias funções diferentes:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para exibir o menu da calculadora
sub exibir_menu {
    print "Escolha uma opção:\n";
    print "1 - Soma\n";
    print "2 - Subtração\n";
    print "3 - Multiplicação\n";
    print "4 - Divisão\n";
    print "5 - Potenciação\n";
    print "6 - Raiz quadrada\n";
    print "7 - Fatorial\n";
    print "0 - Sair\n";
    print "Opção: ";
}

# Função para realizar a soma de dois números
sub somar {
    print "Digite o primeiro número: ";
    my $numero1 = <STDIN>;
    chomp $numero1;
    print "Digite o segundo número: ";
    my $numero2 = <STDIN>;
    chomp $numero2;
    my $resultado = $numero1 + $numero2;
    print "O resultado da soma é: $resultado\n";
}

# Função para realizar a subtração de dois números
sub subtrair {
    print "Digite o primeiro número: ";
    my $numero1 = <STDIN>;
    chomp $numero1;
    print "Digite o segundo número: ";
    my $numero2 = <STDIN>;
    chomp $numero2;
    my $resultado = $numero1 - $numero2;
    print "O resultado da subtração é: $resultado\n";
}

# Função para realizar a multiplicação de dois números
sub multiplicar {
    print "Digite o primeiro número: ";
    my $numero1 = <STDIN>;
    chomp $numero1;
    print "Digite o segundo número: ";
    my $numero2 = <STDIN>;
    chomp $numero2;
    my $resultado = $numero1 * $numero2;
    print "O resultado da multiplicação é: $resultado\n";
}

# Função para realizar a divisão de dois números
sub dividir {
    print "Digite o primeiro número: ";
    my $numero1 = <STDIN>;
    chomp $numero1;
    print "Digite o segundo número: ";
    my $numero2 = <STDIN>;
    chomp $numero2;
    if ($numero2 == 0) {
        print "Erro: divisão por zero!\n";
    } else {
        my $resultado = $numero1 / $numero2;
        print "O resultado da divisão é: $resultado\n";
    }
}

# Função para realizar a potenciação de um número
sub potenciar {
    print "Digite o número base: ";
    my $base = <STDIN>;
    chomp $base;
    print "Digite o expoente: ";
    my $expoente = <STDIN>;
    chomp $expoente;
    my $resultado = $base ** $expoente;
    print "O resultado da potenciação é: $resultado\n";
}

# Função para calcular a raiz quadrada de um número
sub raiz_quadrada {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;
    if ($numero < 0) {
        print "Erro: não é possível calcular a raiz quadrada de um número negativo!\n";
    } else {
        my $resultado = sqrt($numero);
        print "O resultado da raiz quadrada é: $resultado\n";
    }
}

# Função para calcular o fatorial de um número
sub fatorial {
    print "Digite um número: ";
    my $numero = <STDIN>;
    chomp $numero;
    if ($numero < 0) {
        print "Erro: não é possível calcular o fatorial de um número negativo!\n";
    } else {
        my $resultado = 1;
        for (my $i = 1; $i <= $numero; $i++) {
            $resultado *= $i;
        }
        print "O resultado do fatorial é: $resultado\n";
    }
}

# Loop principal
while (1) {
    exibir_menu();
    my $opcao = <STDIN>;
    chomp $opcao;
    if ($opcao == 0) {
        last;
    } elsif ($opcao == 1) {
        somar();
    } elsif ($opcao == 2) {
        subtrair();
    } elsif ($opcao == 3) {
        multiplicar();
    } elsif ($opcao == 4) {
        dividir();
    } elsif ($opcao == 5) {
        potenciar();
    } elsif ($opcao == 6) {
        raiz_quadrada();
    } elsif ($opcao == 7) {
        fatorial();
    } else {
        print "Opção inválida!\n";
    }
}

print "Calculadora encerrada. Até mais!\n";
```

Neste código, foi criada uma calculadora interativa que permite ao usuário escolher entre várias opções de operações matemáticas, como soma, subtração, multiplicação, divisão, potenciação, raiz quadrada e fatorial. O programa exibe um menu para o usuário selecionar a operação desejada e, em seguida, solicita os números necessários para realizar a operação. O resultado é exibido na tela.

O loop principal do programa garante que o usuário possa realizar várias operações consecutivas sem precisar reiniciar o programa a cada vez. A opção "0" encerra o programa e exibe uma mensagem de despedida.

Espero que este código atenda às suas expectativas!