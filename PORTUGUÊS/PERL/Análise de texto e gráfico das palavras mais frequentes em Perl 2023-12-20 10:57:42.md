Claro! Aqui está um exemplo de código em PERL que realiza uma tarefa complexa: a análise de texto para encontrar palavras mais frequentes e exibir um gráfico com os resultados.

```perl
use strict;
use warnings;
use GD::Graph::bars;

# Lê o arquivo de texto
my $texto = ler_arquivo("texto.txt");

# Divide o texto em palavras
my @palavras = split /[\s\.,;:!?\(\)]+/, $texto;

# Cria um hash para contar a frequência das palavras
my %frequencia;
foreach my $palavra (@palavras) {
    next if $palavra eq ""; # Ignora palavras vazias
    $frequencia{$palavra}++;
}

# Ordena as palavras por frequência
my @palavras_ordenadas = sort { $frequencia{$b} <=> $frequencia{$a} } keys %frequencia;

# Seleciona as 10 palavras mais frequentes
my @palavras_top10 = @palavras_ordenadas[0..9];
my @frequencias_top10 = @frequencia{@palavras_top10};

# Imprime as palavras e suas frequências
for my $i (0 .. $#palavras_top10) {
    print "$palavras_top10[$i]: $frequencias_top10[$i]\n";
}

# Gera o gráfico das palavras mais frequentes
my @dados_grafico;
push @dados_grafico, [\@palavras_top10, \@frequencias_top10];
my $grafico = GD::Graph::bars->new(800, 600);
$grafico->set(
    x_label       => 'Palavras',
    y_label       => 'Frequência',
    title         => 'Palavras mais frequentes',
    x_labels_vertical => 1,
    bar_spacing   => 8,
    transparent   => 0,
) or die $grafico->error;
my $imagem_grafico = $grafico->plot(\@dados_grafico);
open(my $imagem_arquivo, '>', 'grafico.png') or die $!;
binmode $imagem_arquivo;
print $imagem_arquivo $imagem_grafico->png;

sub ler_arquivo {
    my ($nome_arquivo) = @_;
    open(my $arquivo, '<', $nome_arquivo) or die $!;
    my $texto = do { local $/; <$arquivo> };
    close($arquivo);
    return $texto;
}
```

Neste exemplo, o código lê um arquivo de texto chamado "texto.txt" e divide o conteúdo em palavras. Em seguida, ele conta a frequência de cada palavra usando um hash. As palavras são então ordenadas por frequência e as 10 palavras mais frequentes são selecionadas.

O código imprime as palavras e suas frequências e, em seguida, gera um gráfico de barras utilizando a biblioteca GD::Graph::bars. O gráfico mostra as 10 palavras mais frequentes no eixo x e a frequência no eixo y. O gráfico é salvo em um arquivo chamado "grafico.png".

O código também inclui uma função auxiliar "ler_arquivo" que lê o conteúdo de um arquivo e retorna como uma string.

Lembre-se de substituir o arquivo de texto "texto.txt" pelo caminho correto para o arquivo que deseja analisar. É necessário ter a biblioteca GD::Graph::bars instalada para executar o código com sucesso.