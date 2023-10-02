Claro! Aqui está um código em Perl que gera um labirinto aleatório utilizando o algoritmo de Recursive Backtracking:

```perl
use strict;
use warnings;

# Define as dimensões do labirinto
my $largura = 20;
my $altura = 10;

# Cria a matriz do labirinto
my @labirinto;
for (my $i = 0; $i < $altura; $i++) {
    for (my $j = 0; $j < $largura; $j++) {
        $labirinto[$i][$j] = 1; # Paredes
    }
}

# Função para gerar o labirinto
sub gerar_labirinto {
    my ($x, $y) = @_;
    $labirinto[$y][$x] = 0; # Marca a célula como caminho

    # Define a ordem aleatória das direções (cima, direita, baixo, esquerda)
    my @direcoes = ( [0, -1], [1, 0], [0, 1], [-1, 0] );
    @direcoes = sort { rand() <=> 0.5 } @direcoes;

    # Tenta avançar em cada direção
    for my $direcao (@direcoes) {
        my ($dx, $dy) = @$direcao;
        my $nx = $x + $dx;
        my $ny = $y + $dy;

        # Verifica se a próxima célula é válida
        if ($nx >= 0 && $nx < $largura && $ny >= 0 && $ny < $altura && $labirinto[$ny][$nx] == 1) {
            $labirinto[$y + $dy / 2][$x + $dx / 2] = 0; # Marca a célula intermediária como caminho
            gerar_labirinto($nx, $ny); # Chama recursivamente para a próxima célula
        }
    }
}

# Chama a função para gerar o labirinto a partir da célula inicial (0, 0)
gerar_labirinto(0, 0);

# Imprime o labirinto
for (my $i = 0; $i < $altura; $i++) {
    for (my $j = 0; $j < $largura; $j++) {
        if ($labirinto[$i][$j] == 1) {
            print "#"; # Parede
        } else {
            print " "; # Caminho
        }
    }
    print "\n";
}
```

Esse código utiliza uma abordagem recursiva para gerar um labirinto aleatório. Primeiro, ele cria uma matriz representando as células do labirinto, onde 1 representa uma parede e 0 representa um caminho. Em seguida, a função `gerar_labirinto` é chamada recursivamente para criar o labirinto. A função utiliza o algoritmo de Recursive Backtracking, que escolhe aleatoriamente uma direção para seguir, marcando as células intermediárias como caminho. O resultado final é impresso na saída padrão, mostrando o labirinto gerado.