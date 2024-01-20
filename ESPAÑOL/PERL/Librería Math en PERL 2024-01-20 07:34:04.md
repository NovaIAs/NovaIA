```perl
use strict;
use warnings;

# Función para calcular el factorial de un número
sub factorial {
  my $n = shift;
  return 1 if $n == 0;
  return $n * factorial($n - 1);
}

# Función para generar un número aleatorio entre dos valores
sub random_range {
  my ($min, $max) = @_;
  return int(rand($max - $min + 1)) + $min;
}

# Función para generar una lista de números aleatorios
sub random_list {
  my ($n, $min, $max) = @_;
  my @list;
  for (1..$n) {
    push @list, random_range($min, $max);
  }
  return @list;
}

# Función para ordenar una lista de números
sub sort_list {
  my @list = @_;
  @list = sort @list;
  return @list;
}

# Función para buscar un elemento en una lista
sub find_element {
  my ($element, @list) = @_;
  my $index = -1;
  for (my $i = 0; $i < @list; $i++) {
    if ($list[$i] == $element) {
      $index = $i;
      last;
    }
  }
  return $index;
}

# Función para eliminar un elemento de una lista
sub remove_element {
  my ($element, @list) = @_;
  my @new_list;
  for (my $i = 0; $i < @list; $i++) {
    if ($list[$i] != $element) {
      push @new_list, $list[$i];
    }
  }
  return @new_list;
}

# Función para calcular la media de una lista de números
sub mean {
  my @list = @_;
  my $sum = 0;
  for (my $i = 0; $i < @list; $i++) {
    $sum += $list[$i];
  }
  return $sum / @list;
}

# Función para calcular la mediana de una lista de números
sub median {
  my @list = @_;
  @list = sort @list;
  my $middle = int(@list / 2);
  if (@list % 2 == 0) {
    return (($list[$middle] + $list[$middle - 1]) / 2);
  } else {
    return $list[$middle];
  }
}

# Función para calcular la moda de una lista de números
sub mode {
  my @list = @_;
  my %counts;
  for (my $i = 0; $i < @list; $i++) {
    $counts{$list[$i]}++;
  }
  my ($mode, $max_count) = (undef, 0);
  for (my $key in %counts) {
    if ($counts{$key} > $max_count) {
      $mode = $key;
      $max_count = $counts{$key};
    }
  }
  return $mode;
}

# Función para calcular la desviación estándar de una lista de números
sub standard_deviation {
  my @list = @_;
  my $mean = mean(@list);
  my $sum_of_squares = 0;
  for (my $i = 0; $i < @list; $i++) {
    $sum_of_squares += ($list[$i] - $mean) ** 2;
  }
  return sqrt($sum_of_squares / (@list - 1));
}

# Función para calcular la varianza de una lista de números
sub variance {
  my @list = @_;
  my $mean = mean(@list);
  my $sum_of_squares = 0;
  for (my $i = 0; $i < @list; $i++) {
    $sum_of_squares += ($list[$i] - $mean) ** 2;
  }
  return $sum_of_squares / (@list - 1);
}

# Función para calcular la correlación entre dos listas de números
sub correlation {
  my (@list1, @list2) = @_;
  my $mean1 = mean(@list1);
  my $mean2 = mean(@list2);
  my $sum_of_products = 0;
  for (my $i = 0; $i < @list1; $i++) {
    $sum_of_products += ($list1[$i] - $mean1) * ($list2[$i] - $mean2);