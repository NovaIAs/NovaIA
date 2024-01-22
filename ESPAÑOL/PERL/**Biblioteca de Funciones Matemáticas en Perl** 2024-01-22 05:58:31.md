```perl
use strict;
use warnings;

sub fibonacci {
  my ($n) = @_;
  return 0 if $n == 0;
  return 1 if $n == 1;
  return fibonacci($n - 1) + fibonacci($n - 2);
}

sub factorial {
  my ($n) = @_;
  return 1 if $n == 0;
  return $n * factorial($n - 1);
}

sub primo {
  my ($n) = @_;
  return 0 if $n < 2;
  return 1 if $n == 2;
  for (my $i = 2; $i <= sqrt($n); $i++) {
    return 0 if $n % $i == 0;
  }
  return 1;
}

sub mayor_comun_divisor {
  my ($a, $b) = @_;
  return $a if $b == 0;
  return mayor_comun_divisor($b, $a % $b);
}

sub menor_comun_multiplo {
  my ($a, $b) = @_;
  return ($a * $b) / mayor_comun_divisor($a, $b);
}

sub permutaciones {
  my ($n, $k) = @_;
  return factorial($n) / factorial($n - $k);
}

sub combinaciones {
  my ($n, $k) = @_;
  return factorial($n) / (factorial($k) * factorial($n - $k));
}

sub potencia {
  my ($base, $exponente) = @_;
  return 1 if $exponente == 0;
  return $base * potencia($base, $exponente - 1);
}

sub raiz_cuadrada {
  my ($n) = @_;
  return sqrt($n);
}

sub convertir_decimal_a_binario {
  my ($n) = @_;
  my @binario;
  while ($n > 0) {
    push @binario, $n % 2;
    $n = int($n / 2);
  }
  return join("", reverse @binario);
}

sub convertir_decimal_a_hexadecimal {
  my ($n) = @_;
  my @hexadecimal;
  while ($n > 0) {
    my $resto = $n % 16;
    push @hexadecimal, chr(65 + $resto) if $resto >= 10;
    push @hexadecimal, $resto;
    $n = int($n / 16);
  }
  return join("", reverse @hexadecimal);
}

sub convertir_binario_a_decimal {
  my ($binario) = @_;
  my $decimal = 0;
  my $exponente = length($binario) - 1;
  for (my $i = 0; $i < length($binario); $i++) {
    my $bit = substr($binario, $i, 1);
    $decimal += $bit * (2 ** $exponente);
    $exponente--;
  }
  return $decimal;
}

sub convertir_hexadecimal_a_decimal {
  my ($hexadecimal) = @_;
  my $decimal = 0;
  my $exponente = length($hexadecimal) - 1;
  for (my $i = 0; $i < length($hexadecimal); $i++) {
    my $digito = substr($hexadecimal, $i, 1);
    my $valor = ord($digito) - 55;
    $valor = $valor - 7 if $valor > 9;
    $decimal += $valor * (16 ** $exponente);
    $exponente--;
  }
  return $decimal;
}

sub ordenar_arreglo {
  my @arreglo = @_;
  my $ordenado = 0;
  while (!$ordenado) {
    $ordenado = 1;
    for (my $i = 0; $i < scalar(@arreglo) - 1; $