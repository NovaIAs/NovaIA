Certamente! Abaixo está um código complexo em Perl que envolve manipulação de arquivos, expressões regulares e iterações:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função para verificar se uma palavra é um palíndromo
sub verificar_palindromo {
   my $palavra = $_[0];
   my $reversa = reverse $palavra;
   if ($palavra eq $reversa) {
      return 1;
   } else {
      return 0;
   }
}

# Função para contar o número de palavras em um arquivo
sub contar_palavras {
   my $arquivo = $_[0];
   open my $fh, '<', $arquivo or die "Não foi possível abrir o arquivo: $!";
   my $contador = 0;
   while (my $linha = <$fh>) {
      my @palavras = split ' ', $linha;
      $contador += scalar(@palavras);
   }
   close $fh;
   return $contador;
}

# Função para extrair endereços de email de um arquivo
sub extrair_emails {
   my $arquivo = $_[0];
   open my $fh, '<', $arquivo or die "Não foi possível abrir o arquivo: $!";
   my @emails;
   while (my $linha = <$fh>) {
      while ($linha =~ /(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)/g) {
         push @emails, $1;
      }
   }
   close $fh;
   return \@emails;
}

# Diretório onde o arquivo será salvo
my $diretorio = "/caminho/do/arquivo";

# Nome do arquivo
my $nome_arquivo = "exemplo.txt";

# Caminho completo do arquivo
my $caminho_arquivo = $diretorio . "/" . $nome_arquivo;

# Criar um arquivo e escrever algumas palavras nele
open my $fh, '>', $caminho_arquivo or die "Não foi possível criar o arquivo: $!";
print $fh "O rato roeu a roupa do rei de Roma.\n";
print $fh "A Rita, a ratazana, roeu a rolha da garrafa da rainha.\n";
print $fh "Um pequeno jabuti xereta viu dez cegonhas felizes.\n";
close $fh;

# Verificar se uma palavra é um palíndromo
my $palavra = "reconhecer";
if (verificar_palindromo($palavra)) {
   print "$palavra é um palíndromo!\n";
} else {
   print "$palavra não é um palíndromo!\n";
}

# Contar o número de palavras no arquivo
my $total_palavras = contar_palavras($caminho_arquivo);
print "O arquivo $nome_arquivo contém $total_palavras palavras.\n";

# Extrair endereços de email do arquivo
my $emails = extrair_emails($caminho_arquivo);
print "Endereços de email encontrados no arquivo:\n";
foreach my $email (@$emails) {
   print "$email\n";
}
```

Este código em Perl contém três funções: "verificar_palindromo", "contar_palavras" e "extrair_emails". 

A função "verificar_palindromo" recebe uma palavra como parâmetro e verifica se ela é um palíndromo (ou seja, se pode ser lida da mesma forma de trás para frente). A função retorna 1 se a palavra for um palíndromo e 0 caso contrário.

A função "contar_palavras" recebe o caminho de um arquivo como parâmetro e conta o número de palavras presentes no arquivo. Ela lê o arquivo linha por linha, divide cada linha em palavras usando o espaço como separador e conta o número total de palavras. A função retorna o número de palavras no arquivo.

A função "extrair_emails" recebe o caminho de um arquivo como parâmetro e extrai todos os endereços de email presentes no arquivo. Ela lê o arquivo linha por linha e usa uma expressão regular para encontrar padrões de endereços de email. Os endereços encontrados são armazenados em um array e retornados pela função.

O código principal cria um arquivo de teste, chama as funções definidas acima e imprime os resultados na tela. No exemplo fornecido, ele verifica se a palavra "reconhecer" é um palíndromo, conta o número de palavras no arquivo "exemplo.txt" e extrai os endereços de email do mesmo arquivo.

Lembre-se de substituir "/caminho/do/arquivo" pelo diretório desejado antes de executar o código. Certifique-se também de ter permissões de escrita no diretório para criar o arquivo de teste.