Claro! Aqui está um exemplo de código em Perl que cria uma calculadora básica com funcionalidades adicionais:

```perl
#!/usr/bin/perl

# Função para somar dois números
sub soma {
    my ($num1, $num2) = @_;
    return $num1 + $num2;
}

# Função para subtrair dois números
sub subtracao {
    my ($num1, $num2) = @_;
    return $num1 - $num2;
}

# Função para multiplicar dois números
sub multiplicacao {
    my ($num1, $num2) = @_;
    return $num1 * $num2;
}

# Função para dividir dois números
sub divisao {
    my ($num1, $num2) = @_;
    
    if ($num2 != 0) {
        return $num1 / $num2;
    }
    else {
        return "Erro: Divisão por zero não é permitida!";
    }
}

# Função para calcular o fatorial de um número
sub fatorial {
    my ($num) = @_;
    my $resultado = 1;
    
    for (my $i = 1; $i <= $num; $i++) {
        $resultado *= $i;
    }
    
    return $resultado;
}

# Função para verificar se um número é primo
sub eh_primo {
    my ($num) = @_;
    
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

# Função principal
sub main {
    print "Bem-vindo à calculadora em Perl!\n\n";
    
    while (1) {
        print "Escolha uma operação:\n";
        print "1. Soma\n";
        print "2. Subtração\n";
        print "3. Multiplicação\n";
        print "4. Divisão\n";
        print "5. Fatorial\n";
        print "6. Verificar se um número é primo\n";
        print "0. Sair\n";
        
        my $opcao = <STDIN>;
        chomp($opcao);
        
        if ($opcao == 0) {
            last;
        }
        elsif ($opcao >= 1 && $opcao <= 4) {
            print "Digite o primeiro número: ";
            my $num1 = <STDIN>;
            chomp($num1);
            
            print "Digite o segundo número: ";
            my $num2 = <STDIN>;
            chomp($num2);
            
            my $resultado;
            
            if ($opcao == 1) {
                $resultado = soma($num1, $num2);
            }
            elsif ($opcao == 2) {
                $resultado = subtracao($num1, $num2);
            }
            elsif ($opcao == 3) {
                $resultado = multiplicacao($num1, $num2);
            }
            elsif ($opcao == 4) {
                $resultado = divisao($num1, $num2);
            }
            
            print "Resultado: $resultado\n\n";
        }
        elsif ($opcao == 5) {
            print "Digite um número: ";
            my $num = <STDIN>;
            chomp($num);
            
            my $resultado = fatorial($num);
            
            print "Fatorial de $num: $resultado\n\n";
        }
        elsif ($opcao == 6) {
            print "Digite um número: ";
            my $num = <STDIN>;
            chomp($num);
            
            my $resultado = eh_primo($num);
            
            if ($resultado) {
                print "$num é primo\n\n";
            }
            else {
                print "$num não é primo\n\n";
            }
        }
        else {
            print "Opção inválida! Por favor, tente novamente.\n\n";
        }
    }
    
    print "Obrigado por usar a calculadora em Perl!\n";
}

main();
```

Este código em Perl cria uma calculadora que permite realizar operações de soma, subtração, multiplicação, divisão, cálculo do fatorial de um número e verificação se um número é primo.

O código começa definindo todas as funções para cada operação matemática e a verificação de número primo. A função `main` é a função principal que é chamada para iniciar a calculadora.

Dentro da função `main`, há um loop infinito que exibe um menu de opções ao usuário e recebe a opção escolhida. Dependendo da opção escolhida, o usuário é solicitado a digitar os números necessários para a operação selecionada.

A função correspondente à operação selecionada é chamada e o resultado é exibido na tela. O loop continua até que o usuário escolha a opção de sair (opção 0).

Espero que este exemplo atenda às suas expectativas! Se você precisar de mais informações ou tiver alguma dúvida, fique à vontade para perguntar.