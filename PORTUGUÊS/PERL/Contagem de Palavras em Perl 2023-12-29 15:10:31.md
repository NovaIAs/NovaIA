Certamente! Aqui está um código complexo em Perl que envolve manipulação de strings e estruturas de controle. O código abaixo foi projetado para receber uma frase como entrada e, em seguida, realizar a contagem do número de ocorrências de cada palavra na frase:

```perl
use strict;
use warnings;

sub contar_palavras {
    my ($frase) = @_;
    
    # Remover pontuação e converter para minúsculas
    $frase =~ s/[[:punct:]]//g;
    $frase = lc $frase;
    
    # Criar um hash vazio para armazenar as contagens
    my %contagem;
    
    # Dividir a frase em palavras e contar ocorrências
    foreach my $palavra (split /\s+/, $frase) {
        $contagem{$palavra}++;
    }
    
    # Imprimir resultados
    print "Contagem de palavras:\n";
    foreach my $palavra (keys %contagem) {
        printf "%-10s: %d\n", $palavra, $contagem{$palavra};
    }
}

# Solicitar entrada do usuário
print "Digite uma frase: ";
my $frase = <STDIN>;
chomp $frase;

# Chamar a função contar_palavras
contar_palavras($frase);
```

Explicação do código:

1. Começamos importando os módulos "strict" e "warnings" para garantir que nosso código esteja livre de erros e avisos.

2. Em seguida, definimos uma função chamada "contar_palavras" que recebe uma frase como entrada.

3. Dentro da função, usamos expressões regulares para remover a pontuação da frase e convertê-la para minúsculas.

4. Criamos um hash vazio chamado "%contagem" para armazenar as contagens de palavras.

5. Em seguida, dividimos a frase em palavras usando a função "split" e, para cada palavra, incrementamos sua contagem no hash "%contagem".

6. Depois disso, imprimimos os resultados percorrendo as chaves do hash "%contagem" e, para cada palavra, imprimimos a palavra e o número de ocorrências usando a função "printf".

7. No trecho principal do código, solicitamos ao usuário que digite uma frase e armazenamos a entrada em uma variável chamada "$frase". Em seguida, removemos a quebra de linha utilizando a função "chomp".

8. Por fim, chamamos a função "contar_palavras" passando a frase como argumento.

Espero que este código complexo em Perl atenda às suas expectativas! Lembre-se de que é apenas um exemplo e pode ser adaptado conforme necessário.