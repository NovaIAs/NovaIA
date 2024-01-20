```perl
use strict;
use warnings;

my $texto = "Hola mundo, este es un ejemplo de código Perl.";

# Dividir el texto en una lista de palabras
my @palabras = split /\s+/, $texto;

# Crear un mapa de palabras con sus frecuencias
my %frecuencias;
for my $palabra (@palabras) {
    $frecuencias{$palabra}++;
}

# Obtener la lista de palabras únicas
my @palabras_unicas = keys %frecuencias;

# Ordenar la lista de palabras únicas por frecuencia descendente
my @palabras_ordenadas = sort { $frecuencias{$b} <=> $frecuencias{$a} } @palabras_unicas;

# Imprimir la lista de palabras únicas y sus frecuencias
print "Palabras únicas y sus frecuencias:\n";
for my $palabra (@palabras_ordenadas) {
    print "$palabra: $frecuencias{$palabra}\n";
}

# Encontrar la palabra más frecuente
my $palabra_mas_frecuente = $palabras_ordenadas[0];

# Imprimir la palabra más frecuente
print "Palabra más frecuente: $palabra_mas_frecuente\n";

# Encontrar la palabra menos frecuente
my $palabra_menos_frecuente = $palabras_ordenadas[-1];

# Imprimir la palabra menos frecuente
print "Palabra menos frecuente: $palabra_menos_frecuente\n";

# Calcular la frecuencia promedio de las palabras
my $frecuencia_promedio = 0;
for my $palabra (@palabras_unicas) {
    $frecuencia_promedio += $frecuencias{$palabra};
}
$frecuencia_promedio /= scalar @palabras_unicas;

# Imprimir la frecuencia promedio de las palabras
print "Frecuencia promedio de las palabras: $frecuencia_promedio\n";
```

Explicación del código:

1. La primera línea "use strict;" activa el modo estricto en Perl, lo que ayuda a detectar errores en el código.
2. La segunda línea "use warnings;" activa las advertencias en Perl, lo que ayuda a identificar posibles problemas en el código.
3. La tercera línea "my $texto = "Hola mundo, este es un ejemplo de código Perl.";" define una variable llamada "$texto" y le asigna el valor "Hola mundo, este es un ejemplo de código Perl.".
4. La cuarta línea "my @palabras = split /\s+/, $texto;" divide el texto en una lista de palabras usando el espacio en blanco como separador.
5. La quinta línea "my %frecuencias;" define un mapa de palabras con sus frecuencias.
6. El siguiente bucle "for my $palabra (@palabras) { $frecuencias{$palabra}++; }" itera sobre cada palabra en la lista "@palabras" y incrementa la frecuencia de la palabra en el mapa "%frecuencias".
7. La octava línea "my @palabras_unicas = keys %frecuencias;" obtiene la lista de palabras únicas del mapa "%frecuencias".
8. La novena línea "my @palabras_ordenadas = sort { $frecuencias{$b} <=> $frecuencias{$a} } @palabras_unicas;" ordena la lista de palabras únicas por frecuencia descendente.
9. El siguiente bucle "for my $palabra (@palabras_ordenadas) { print "$palabra: $frecuencias{$palabra}\n"; }" imprime la lista de palabras únicas y sus frecuencias.
10. La decimoprimera línea "my $palabra_mas_frecuente = $palabras_ordenadas[0];" obtiene la palabra más frecuente de la lista "@palabras_ordenadas".
11. La decimosegunda línea "print "Palabra más frecuente: $palabra_mas_frecuente\n";" imprime la palabra más frecuente.
12. La decimotercera línea "my $palabra_menos_frecuente = $palabras_ordenadas[-1];" obtiene la palabra menos frecuente de la lista "@palabras_ordenadas".
13. La decimocuarta línea "print "Palabra menos frecuente: $palabra_menos_frecuente\n";" imprime la palabra menos frecuente.
14. El siguiente bucle "for my $palabra (@palabras_unicas) { $frecuencia_promedio += $frecuencias{$palabra}; }" calcula la frecuencia promedio de las palabras.
15. La decimosexta línea "$frecuencia_promedio /= scalar @palabras_unicas;" divide la frecuencia promedio por el número de palabras únicas para obtener el valor promedio.
16. La decimoséptima línea "print "Frecuencia promedio de las palabras: $frecuencia_promedio\n";" imprime la frecuencia promedio de las palabras.