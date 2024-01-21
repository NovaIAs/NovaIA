```perl
# Perl (Comunidad de Practicantes de Perl) en Español

# Este programa lee un archivo de texto y cuenta el número de palabras, líneas y caracteres que contiene.

# Declaración de variables
mi $nombre_archivo = 'nombre-de-archivo.txt';
mi $archivo;
mi $linea;
mi $contador_palabras = 0;
mi $contador_lineas = 0;
mi $contador_caracteres = 0;

# Abrir el archivo
abrir($archivo, '<', $nombre_archivo) || die "No se pudo abrir el archivo $nombre_archivo: $!";

# Leer el archivo línea por línea
while ($linea = <$archivo>) {
    # Contar el número de palabras en la línea
    $contador_palabras += $linea =~ s/\s+/ /gr;

    # Contar el número de líneas
    $contador_lineas++;

    # Contar el número de caracteres
    $contador_caracteres += length($linea);
}

# Cerrar el archivo
close($archivo);

# Imprimir los resultados
print "El archivo $nombre_archivo contiene:\n";
print "Palabras: $contador_palabras\n";
print "Líneas: $contador_lineas\n";
print "Caracteres: $contador_caracteres\n";

```

Explicación del código:

* **Declaración de variables**: Se declaran las siguientes variables:
    * `$nombre_archivo`: El nombre del archivo de texto que se desea analizar.
    * `$archivo`: El objeto que representa el archivo abierto.
    * `$linea`: La variable que contendrá cada línea del archivo.
    * `$contador_palabras`: El número de palabras en el archivo.
    * `$contador_lineas`: El número de líneas en el archivo.
    * `$contador_caracteres`: El número de caracteres en el archivo.
* **Abrir el archivo**: La función `open()` se utiliza para abrir el archivo especificado por `$nombre_archivo`. El primer argumento de la función es el objeto que representa el archivo, el segundo argumento es el modo de apertura (en este caso, lectura) y el tercer argumento es el nombre del archivo.
* **Leer el archivo línea por línea**: El bucle `while` se utiliza para leer el archivo línea por línea. La variable `$linea` se asigna a la línea actual del archivo.
* **Contar el número de palabras en la línea**: La expresión regular `s/\s+/ /gr` se utiliza para sustituir todos los espacios consecutivos por un solo espacio. El resultado de la sustitución se asigna a la variable `$contador_palabras`.
* **Contar el número de líneas**: La variable `$contador_lineas` se incrementa en 1 cada vez que se lee una nueva línea del archivo.
* **Contar el número de caracteres**: La función `length()` se utiliza para obtener el número de caracteres en la línea actual. El resultado se asigna a la variable `$contador_caracteres`.
* **Cerrar el archivo**: La función `close()` se utiliza para cerrar el archivo.
* **Imprimir los resultados**: La función `print()` se utiliza para imprimir los resultados del análisis del archivo.